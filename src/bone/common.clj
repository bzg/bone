;; Copyright (c) 2026 Bastien Guerry <bzg@gnu.org>
;; SPDX-License-Identifier: EPL-2.0
;; License-Filename: LICENSES/EPL-2.0.txt

(ns bone.common
  "Shared utilities for BONE.  The invariant is: no datalevin dependency,
  loadable by both JVM and Babashka.  Mostly pure functions, plus a few
  file readers (read-failures-file...).  Config loading lives in
  `bone.config`, iCalendar handling in `bone.ics`, and what only the
  bb scripts need in `bone.export`."
  (:require [clojure.string :as str]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [taoensso.timbre :as log])
  (:import [java.text Normalizer Normalizer$Form]
           [java.security MessageDigest]
           [java.time LocalDate ZoneId]
           [java.time.format DateTimeFormatter]
           [java.util Date]))

;; ---------------------------------------------------------------------------
;; Constants
;; ---------------------------------------------------------------------------

(def bone-schema
  (edn/read-string (slurp (io/resource "bone-schema.edn"))))

(def failures-file-path
  "Shared command-failures EDN file: JVM writes, bb scripts read.
   Kept under data/ so the public/ directory only holds files that
   are safe to serve."
  "data/.failures.edn")

(defn read-failures-file
  "Read the failures EDN file at `path`, returning a vector ([] if
  missing or unparseable).  `on-error` is invoked with the exception
  on parse failure."
  ([path] (read-failures-file path nil))
  ([path on-error]
   (let [f (io/file path)]
     (if (.exists f)
       (try (edn/read-string (slurp f))
            (catch Exception e
              (when on-error (on-error e))
              []))
       []))))

;; ---------------------------------------------------------------------------
;; Pure utilities
;; ---------------------------------------------------------------------------

(defn expand-home
  "Expand a leading ~/ to the user's home directory."
  [p]
  (if (and (string? p) (str/starts-with? p "~/"))
    (str (System/getProperty "user.home") (subs p 1))
    p))

;; Shared identifier rule for :sources [{:name ...}] and :mailboxes
;; [{:name ...}].  Starts with alphanum; spaces allowed in the middle
;; (but not trailing) to keep current :source/name configs valid; no
;; slash / colon / control chars (those would break paths or
;; watermark ids).
(def config-name-regex #"[a-zA-Z0-9](?:[a-zA-Z0-9 ._-]*[a-zA-Z0-9._-])?")

(defn valid-config-name?
  "True iff `s` is a non-blank string matching `config-name-regex`."
  [s]
  (and (string? s)
       (not (str/blank? s))
       (some? (re-matches config-name-regex s))))

(def singleton-mailbox-error
  "Single source of truth for the rejected-`:mailbox`-key message."
  (str ":mailbox is no longer accepted (even when set to nil) "
       "-- use :mailboxes [{...}] (vector of mailbox maps with a :name each)."))

(defn all-distinct?
  "True iff `coll` contains no duplicates (under `=`).  Prefer over
  `(apply distinct? coll)` -- the latter throws on empty collections
  and depends on splatting through `apply`."
  [coll]
  (= (count coll) (count (distinct coll))))

(defn slugify
  "Normalize a source name for use as a directory name."
  [s]
  (-> (Normalizer/normalize (str s) Normalizer$Form/NFD)
      (str/replace #"\p{InCombiningDiacriticalMarks}+" "")
      str/lower-case
      (str/replace #"[^a-z0-9]+" "-")
      (str/replace #"^-|-$" "")))

(defn sha256
  "Compute SHA-256 hex digest of a string."
  ^String [^String s]
  (let [digest (MessageDigest/getInstance "SHA-256")
        bytes  (.digest digest (.getBytes s "UTF-8"))]
    (str/join (map #(format "%02x" (Byte/toUnsignedInt %)) bytes))))

(defn mid-hash
  "Stable directory-safe hash of a message-id; also the value of the
  :*/message-id-hash unique attrs every mid lookup goes through
  (raw mids can exceed LMDB's key limit).  Nil-safe."
  [message-id]
  (when message-id
    (sha256 (str "bone:" message-id))))

(def patch-filename-re #"(?i)\.(patch|diff)$")

(defn patch-file?
  "True if filename looks like a patch/diff file."
  [filename]
  (boolean (and filename (re-find patch-filename-re filename))))

(def text-content-types
  "Content types whose attachment data is stored as text."
  #{"text/plain" "text/x-log"})

(defn text-attachment?
  "True if an attachment has a text/plain or text/x-log content type
  and is not a patch: a .patch/.diff file is published as a patch
  whatever its content type (some mailers send it as text/plain), and
  must not show up a second time as a text attachment."
  [att]
  (boolean (when-let [ct (:attachment/content-type att)]
             (and (not (patch-file? (:attachment/filename att)))
                  (text-content-types (-> ct str/lower-case (str/split #";") first str/trim))))))

;; ---------------------------------------------------------------------------
;; Email body text and git trailers
;; ---------------------------------------------------------------------------

(defn strip-signature
  "Remove the RFC 3676 email signature (everything after a line
  containing exactly \"-- \") from plain-text body."
  [text]
  (if text
    (first (str/split text #"\r?\n-- \s*(?:\r?\n|$)" 2))
    text))

(defn email-body-text
  "Return the plain-text body of an email, preferring :email/body-text
  over :email/body-text-from-html.  The RFC 3676 signature delimiter
  (\"-- \") is stripped so that signature lines are not scanned for
  commands."
  [email]
  (some-> (or (:email/body-text email) (:email/body-text-from-html email))
          strip-signature))

(def trailer-keys
  "Person trailers collected from replies to a patch, b4-style:
  lowercase key -> canonical capitalization.  Distinct from the BONE
  command layer (`Acked-by:` as a maintainer proxy command): trailers
  are collected verbatim from anyone, for clients to fold into the
  commit message when applying the patch."
  {"acked-by"        "Acked-by"
   "reviewed-by"     "Reviewed-by"
   "tested-by"       "Tested-by"
   "reported-by"     "Reported-by"
   "suggested-by"    "Suggested-by"
   "signed-off-by"   "Signed-off-by"
   "co-developed-by" "Co-developed-by"})

(def ^:private patch-start-line-re
  "First line of a patch pasted inline in a reply (mirrors detect's
  inline-patch-start-patterns, which bone.detect cannot share without
  inverting the dependency).  extract-trailers stops there, so a fixup
  pasted below the reply text cannot leak its own Signed-off-by."
  #"^(?:From [0-9a-f]{40} |diff --git |--- a/)")

(defn lines-before-patch
  "Lines of `body` that precede the first line of a patch pasted
  inline.  What follows is patch content, not something the sender
  says: its trailers, and the \"+1\" or \"-1\" of its hunk headers,
  must not be read as the sender's."
  [body]
  (take-while #(not (re-find patch-start-line-re %)) (str/split-lines body)))

(defn extract-trailers
  "Collect git person trailers from a reply body.
  Returns a distinct vector of \"Key: Name <addr>\" strings for the
  lines whose key starts at column 0 and is in `trailer-keys`, and
  whose value contains an @ (so prose like \"Tested-by: nobody yet\"
  is skipped).  Quoted lines never match, their key sitting behind
  \">\".  Scanning stops at the first line of a pasted patch, the way
  b4 does.  Keys are canonicalized, values trimmed.  Nil-safe on body."
  [body]
  (when body
    (->> (lines-before-patch body)
         (keep (fn [line]
                 (when-let [[_ k v] (re-matches #"([A-Za-z][A-Za-z-]*):\s*(\S.*?)\s*" line)]
                   (when-let [ck (get trailer-keys (str/lower-case k))]
                     (when (str/includes? v "@")
                       (str ck ": " v))))))
         distinct
         vec)))

;; ---------------------------------------------------------------------------
;; Date formatting
;; ---------------------------------------------------------------------------

;; Thread-safe formatter (DateTimeFormatter is immutable and safe to share).
(def ^:private date-iso-fmt
  (-> (DateTimeFormatter/ofPattern "yyyy-MM-dd")
      (.withZone (ZoneId/of "UTC"))))

(defn format-date-iso
  "Format a java.util.Date as yyyy-MM-dd in UTC."
  [^Date date]
  (when date
    (.format date-iso-fmt (.toInstant date))))

(defn days-between
  "Number of whole days between two Dates (absolute)."
  [^Date a ^Date b]
  (when (and a b)
    (quot (Math/abs (- (.getTime b) (.getTime a))) 86400000)))

;; ---------------------------------------------------------------------------
;; Duration parsing
;; ---------------------------------------------------------------------------

(def ^:private duration-unit-days
  {"y" 365 "m" 30 "w" 7 "d" 1})

(defn parse-duration-str
  "Parse \"2y 3m 10d 2w\" into a total number of days.
  Returns nil if no valid tokens. Throws on unrecognized units or
  trailing junk after at least one valid token."
  [s]
  (let [valid     (re-seq #"(\d+)\s*(y|m|w|d)" s)
        bad-units (->> (re-seq #"(\d+)\s*([a-zA-Z]+)" s)
                       (remove (fn [[_ _ u]] (contains? duration-unit-days u)))
                       (map #(nth % 2)))
        leftover  (some-> s
                           (str/replace #"(\d+)\s*(y|m|w|d)" "")
                           str/trim)]
    (when (seq bad-units)
      (throw (ex-info (str "Unknown duration unit(s): " (str/join ", " bad-units)
                           " (expected y, m, w, d)")
                      {:value s :bad-units (vec bad-units)})))
    (when (and (seq valid) (seq leftover))
      (throw (ex-info (str "Invalid duration syntax: " (pr-str s))
                      {:value s :leftover leftover})))
    (when (seq valid)
      (reduce (fn [acc [_ n unit]]
                (+ acc (* (parse-long n) (duration-unit-days unit))))
              0 valid))))

(defn parse-delay
  "Parse a duration value into a number of days.
  Accepts an integer (days) or a duration string (\"30d\", \"6w\", \"3m\").
  Note: \"0d\" returns 0, meaning 'expire immediately'."
  [v]
  (cond
    (integer? v) v
    (string? v)  (parse-duration-str v)
    :else        nil))

(defn parse-iso-date
  "Parse an ISO date string (\"2024-01-01\") into a java.util.Date at
  UTC midnight, or nil.  UTC keeps period bounds, expiry rules and
  deadlines consistent with format-date-iso, whatever the local TZ."
  [^String s]
  (when (and s (re-matches #"\d{4}-\d{2}-\d{2}" s))
    (try
      (Date/from (.toInstant (.atStartOfDay (LocalDate/parse s)
                                            (ZoneId/of "UTC"))))
      (catch Exception _ nil))))

;; ---------------------------------------------------------------------------
;; Header utilities
;; ---------------------------------------------------------------------------

(defn parse-headers
  "Parse headers-edn into a seq of [key value] pairs.
  Accepts a string (EDN) or an already-parsed collection. Returns nil on failure."
  [headers-edn]
  (when headers-edn
    (try
      (if (string? headers-edn) (edn/read-string headers-edn) headers-edn)
      (catch Exception e
        (log/warn "Failed to parse headers-edn:" (.getMessage e))
        nil))))

(defn get-header
  "Case-insensitive header lookup from headers (string, map, or parsed seq).
  Returns the first value (string) or a vector if the header appears multiple times."
  [headers-edn header-name]
  (when-let [headers (parse-headers headers-edn)]
    (let [lname (str/lower-case header-name)]
      (some (fn [[k v]] (when (= (str/lower-case k) lname) v)) headers))))

(defn get-header-values
  "Like get-header but always returns a vector of strings (empty if missing).
  Handles headers stored as a single string or as a vector of strings."
  [headers-edn header-name]
  (let [v (get-header headers-edn header-name)]
    (cond
      (nil? v)    []
      (vector? v) v
      :else       [v])))

;; Regex source for an email address in a command line.  It holds no
;; separator (, ;), parenthesis or double quote, does not start or end
;; with ', nor end with sentence punctuation: "Alice (a@x.org), b@y.org."
;; yields a@x.org and b@y.org.  A dot is required in the domain.
(def address-re
  (str "[^@\\s<>,;()\"'][^@\\s<>,;()\"]*"
       "@[^@\\s<>,;()\"]+\\.[^@\\s<>,;()\"]*[^@\\s<>,;()\".:?!']"))

;; RFC 5322 §3.6.4 token shape -- msg-id = "<" id-left "@" id-right ">".
;; Reject whitespace and angle brackets inside; tolerate the obs-* set
;; through the broad atext-ish character class.
(def ^:private mid-token-re #"<[^<>\s]+>")

(defn normalize-mid
  "Normalize a bracketed Message-Id by lowercasing the domain part
  (everything after the last `@`), per RFC 5322 §3.6.4 which makes the
  id-right case-insensitive while keeping the id-left case-sensitive.
  Returns nil on nil; leaves mids without `@` untouched."
  [mid]
  (when mid
    (let [at (.lastIndexOf ^String mid (int \@))]
      (if (pos? at)
        (str (subs mid 0 (inc at)) (str/lower-case (subs mid (inc at))))
        mid))))

(defn bracketed-token
  "First <addr@domain> token in v (string or vector); nil otherwise.
  Case preserved: `extract-bracketed-id` lowercases the domain,
  `bone.export/extract-bracketed-id-raw` does not."
  [v]
  (when v
    (let [s (if (vector? v) (first v) (str v))]
      (when-not (str/blank? s)
        (first (re-seq mid-token-re s))))))

(defn extract-bracketed-id
  "Extract the first <addr@domain> token from a header value, domain
  lowercased (RFC 5322 §3.6.4)."
  [v]
  (some-> (bracketed-token v) normalize-mid))

(defn extract-in-reply-to
  "Extract In-Reply-To message-id from a headers map (raw or parsed).
  Handles both string and vector values."
  [headers]
  (extract-bracketed-id (get-header headers "In-Reply-To")))

;; Purely defensive (identity uses the fixed-length mid-hash): a
;; multi-KB Message-Id is hostile input, skipped cleanly at ingest so
;; a failing transact can never block the watermark.
(def ^:const max-mid-length 10000)

(defn ancestor-mids-from
  "Ordered vector of ancestor mids (root first, parent last) from
  References + In-Reply-To.  Mids are normalized and deduped."
  [references in-reply-to]
  (let [refs (if (string? references)
               (mapv normalize-mid (re-seq mid-token-re references))
               [])
        irt  (normalize-mid in-reply-to)
        all  (if (and irt (not (some #{irt} refs)))
               (conj (vec refs) irt)
               (vec refs))]
    (into [] (distinct) all)))

;; ---------------------------------------------------------------------------
;; Source classification
;; ---------------------------------------------------------------------------

(defn source-type
  "Infer the source type from its config keys. Returns nil if none present."
  [source]
  (cond
    (:list source)  :mailing-list
    (:alias source) :alias
    (:to source)    :mailbox))

(defn sent-via-source-channel?
  "True when the email was delivered through the source's public channel,
  or when the source is a mailbox (no public/private distinction)."
  [delivery source-cfg]
  (case (:source-type source-cfg)
    :mailing-list (= :list delivery)
    :alias        (= :alias delivery)
    :mailbox      true
    false))

(defn- header-token
  "Bare, lower-cased identifier of a header value or config key:
  `Name <a@b>` and `<a@b>` give \"a@b\"."
  [v]
  (let [s (str v)]
    (str/lower-case (str/trim (or (second (re-find #"<([^>]+)>" s)) s)))))

(defn- any-matches?
  "True if a header value in `coll` equals the source key `expected`,
  case aside.  Exact, not substring: :list \"gnu-emacs\" must not
  capture help-gnu-emacs.gnu.org, nor :alias bugs@x.org debbugs@x.org."
  [coll expected]
  (let [e (header-token expected)]
    (boolean (some #(= e (header-token %)) coll))))

(defn- header-addresses
  "Every value of `header-name`, repeated headers included, with
  comma-separated lists split (Exim's `Envelope-to: a@x, b@x`)."
  [headers-edn header-name]
  (->> (get-header-values headers-edn header-name)
       (mapcat #(str/split (str %) #","))
       (remove str/blank?)))

(def ^:private original-recipient-headers
  ["X-Original-To" "Envelope-To" "X-Envelope-To"])

(defn- multiple-delivered-to?
  "True when Delivered-To contains 2+ distinct addresses (alias signal)."
  [headers-edn]
  (let [vals (->> (get-header-values headers-edn "Delivered-To")
                  (map str/lower-case)
                  distinct)]
    (> (count vals) 1)))

(defn- original-recipient-not-in-to-cc?
  "True when an original recipient (any value of X-Original-To,
  Envelope-To, X-Envelope-To) appears in neither To nor Cc."
  [hdrs]
  (let [to-cc (into #{} (comp (mapcat #(header-addresses hdrs %))
                              (map header-token))
                    ["To" "Cc"])]
    (boolean (some #(not (contains? to-cc (header-token %)))
                   (mapcat #(header-addresses hdrs %) original-recipient-headers)))))

(defn classify-delivery
  "Heuristic: returns :list, :alias, or :direct based on email headers."
  [headers-edn]
  (let [hdrs (parse-headers headers-edn)]
    (cond
      ;; 1. Any List-Id or X-BeenThere => mailing list
      (or (get-header hdrs "List-Id")
          (get-header hdrs "X-BeenThere"))
      :list

      ;; 2a. Original recipient not in To/Cc => alias
      (original-recipient-not-in-to-cc? hdrs)
      :alias

      ;; 2b. Multiple distinct Delivered-To => alias
      (multiple-delivered-to? hdrs)
      :alias

      ;; 3. Otherwise => direct
      :else :direct)))

(defn- match-source?
  "Check if headers match a source based on its inferred type."
  [headers-edn source]
  (case (source-type source)
    :mailing-list (any-matches? (get-header-values headers-edn "List-Id")
                                (:list source))
    :alias        (any-matches? (mapcat #(header-addresses headers-edn %)
                                        (conj original-recipient-headers
                                              "Delivered-To"))
                                (:alias source))
    :mailbox      (any-matches? (header-addresses headers-edn "Delivered-To")
                                (:to source))
    false))

(defn classify-source
  "Return the :name of the first matching source, or nil.
  Uses header-based matching only (List-Id; X-Original-To, Envelope-To,
  X-Envelope-To, Delivered-To)."
  [headers-edn sources]
  (some (fn [source]
          (when (match-source? headers-edn source) (:name source)))
        sources))

;; ---------------------------------------------------------------------------
;; Author resolution
;; ---------------------------------------------------------------------------

(defn- list-munged-from?
  "True when From appears to have been rewritten by the list MTA
  (Mailman/DMARC mitigation).  Mailman wraps the original display
  name as `Original Name via List-Display-Name`, so a literal
  ` via ` token in the From-name is the universal munging marker."
  [from-name]
  (boolean (and from-name (re-find #"(?i) via " from-name))))

(defn resolve-author
  "Effective author {:address :name} of an email.  When the From has
  been DMARC-munged by the list (detected by \" via \" in the display
  name) the real author is in Reply-To; otherwise From wins."
  [{:keys [from-address from-name reply-to]}]
  (let [rto-addr (:address (first reply-to))
        rto-name (:name    (first reply-to))]
    (if (and (list-munged-from? from-name)
             rto-addr
             (not= (some-> rto-addr str/lower-case)
                   (some-> from-address str/lower-case)))
      {:address rto-addr
       :name    (or rto-name from-name)}
      {:address from-address
       :name    from-name})))

;; ---------------------------------------------------------------------------
;; Label / command defaults and merge logic
;; ---------------------------------------------------------------------------

(def report-type-spec
  "Ordered spec of each report type.  Vector order drives subject
  detection precedence and per-type export iteration.  :type is the
  keyword identifier, :tags the default subject tokens, :plural the
  noun used in exports, :versioned? whether the bracket's last token
  is parsed as :version.  :patch has a dedicated parser in bone.detect."
  [{:type :bug          :tags ["BUG"]                :plural "bugs"}
   {:type :patch        :tags ["PATCH"]              :plural "patches"}
   {:type :request      :tags ["POLL" "TODO"]        :plural "requests"}
   {:type :announcement :tags ["ANN" "ANNOUNCEMENT"] :plural "announcements"}
   {:type :release      :tags ["REL" "RELEASE"]      :plural "releases"       :versioned? true}
   {:type :change       :tags ["CHG" "CHANGE"]       :plural "changes"        :versioned? true}])

(def default-labels (into {} (map (juxt :type :tags)) report-type-spec))

(def default-commands
  ;; No bare "Reviewed" in :acked: in loose mode "Reviewed v2 and
  ;; found problems." would ack with inverse polarity.  The kernel
  ;; idiom is the Reviewed-by: line (syntax synonym of Acked-by:).
  {:acked     ["Acked" "Confirmed" "Approved"]
   :owned     ["Owned"]
   :closed    ["Canceled" "Cancelled" "Closed" "Expired"
               "Resolved" "Applied" "Completed" "Fixed"]
   :urgent    ["Urgent"]
   :important ["Important"]})

(def close-reasons
  {"Canceled"  :canceled
   "Cancelled" :canceled
   "Expired"   :expired})

(defn bang-prefix
  "Regex fragment for the `!` prefix on Bone instructions.
  Required when `strict-syntax?` is true, optional otherwise."
  [strict-syntax?]
  (if strict-syntax? "!" "!?"))

(defn resolve-labels-map [source-cfg]
  (cond-> default-labels
    (:global-labels source-cfg) (merge (:global-labels source-cfg))
    (:labels source-cfg)        (merge (:labels source-cfg))))

(defn- validate-word-entry
  "Validate a :words entry. Must be a bare string."
  [entry]
  (if (string? entry)
    entry
    (throw (ex-info (str "Invalid :words entry: " (pr-str entry)
                         " (expected a bare string)")
                    {:entry entry}))))

(defn- extract-words-map
  "Return {cmd-id words} for entries that carry a :words vector."
  [commands-map]
  (reduce-kv (fn [acc k v]
               (if-let [w (:words v)] (assoc acc k w) acc))
             {} commands-map))

(defn- merged-raw-words
  "Return {cmd-id [strings]}.  Defaults merged first, then global, then
  local commands."
  [source-cfg]
  (merge default-commands
         (extract-words-map (:global-commands source-cfg))
         (extract-words-map (:commands source-cfg))))

(defn resolve-commands-map
  "Return {cmd-id [strings]} with every entry validated as a bare string."
  [source-cfg]
  (update-vals (merged-raw-words source-cfg)
               #(mapv validate-word-entry %)))

(defn- extract-overrides-map
  "Keep only :scope and :report-types for each command, dropping entries
  with no override."
  [commands-map]
  (reduce-kv (fn [acc k v]
               (let [overrides (select-keys v [:scope :report-types])]
                 (if (seq overrides) (assoc acc k overrides) acc)))
             {} commands-map))

(defn resolve-command-overrides [source-cfg]
  (merge (extract-overrides-map (:global-commands source-cfg))
         (extract-overrides-map (:commands source-cfg))))

(defn resolve-command-syntax
  "Return :loose or :strict for the source."
  [source-cfg]
  (case (:command-syntax source-cfg)
    nil     :loose
    :loose  :loose
    :strict :strict
    (throw (ex-info (str "Invalid :command-syntax: "
                         (pr-str (:command-syntax source-cfg))
                         " (expected :loose or :strict)")
                    {:value (:command-syntax source-cfg)}))))

(defn patch-triggers?
  "True when patches on this source auto-credit acked/owned on the
  parent bug/request and propagate :resolved closure.  Default true;
  source-level \":patch-triggers? false\" opts out."
  [source-cfg]
  (let [v (:patch-triggers? source-cfg)]
    (if (nil? v) true (boolean v))))

;; ---------------------------------------------------------------------------
;; Maintainer tenures (pure -- operate on a seq of tenure maps, no DB access)
;; ---------------------------------------------------------------------------
;;
;; A "tenure" is a map {:email str :from Date|nil :to Date|nil :order long|nil}
;; representing one contiguous period during which an address held maintainer
;; status on a source. :from nil = active since the beginning of time.
;; :to nil = the tenure is currently active. :order encodes the config order
;; used as a tie-break when computing the lead maintainer.

(defn- tenure-sort-key [t]
  [(if-let [^Date f (:from t)] (.getTime f) 0)
   (or (:order t) Long/MAX_VALUE)
   (str/lower-case (or (:email t) ""))])

(defn lead-maintainer
  "Return the lower-cased email of the lead maintainer -- the active tenure
  with the earliest :from (nil sorts first), tie-broken by :order then email.
  Returns nil if no active tenure."
  [tenures]
  (:email (first (sort-by tenure-sort-key (filter #(nil? (:to %)) tenures)))))

(defn lead-maintainer?
  "True if addr is the current lead maintainer for this source."
  [tenures addr]
  (and addr
       (when-let [lead (lead-maintainer tenures)]
         (= (str/lower-case addr) lead))))

(defn maintainer?
  "True if `addr` has maintainer status.  2-arity: any active tenure;
  3-arity: tenure covering `as-of` (nil bounds = unbounded).  A nil
  `as-of` falls back to the 2-arity for emails missing a Date header."
  ([tenures addr]
   (and addr
        (let [a (str/lower-case addr)]
          (boolean (some #(and (= a (:email %)) (nil? (:to %))) tenures)))))
  ([tenures addr as-of]
   (if (nil? as-of)
     (maintainer? tenures addr)
     (and addr
          (let [a (str/lower-case addr)]
            (boolean
             (some (fn [{:keys [email from to]}]
                     (and (= a email)
                          (or (nil? from) (not (.before ^Date as-of ^Date from)))
                          (or (nil? to)   (.before ^Date as-of ^Date to))))
                   tenures)))))))

;; ---------------------------------------------------------------------------
;; Report scoring
;; ---------------------------------------------------------------------------

(defn report-priority [report]
  (+ (if (:report/urgent report) 2 0)
     (if (:report/important report) 1 0)))

;; ---------------------------------------------------------------------------
;; Tenure pull pattern
;; ---------------------------------------------------------------------------

;; Pull pattern for maintainer tenures. Shared between JVM (roles.clj) and
;; bb (scripts/bone/common_bb.clj) so both sides consume the same attribute list.
(def tenure-pull-pattern
  '[:db/id :maint-tenure/email :maint-tenure/from :maint-tenure/to :maint-tenure/order])

(defn tenure-map
  "Convert a raw Datalevin pull result into a tenure map."
  [m]
  {:eid   (:db/id m)
   :email (:maint-tenure/email m)
   :from  (:maint-tenure/from m)
   :to    (:maint-tenure/to m)
   :order (:maint-tenure/order m)})
