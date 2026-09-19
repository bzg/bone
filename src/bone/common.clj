;; Copyright (c) 2026 Bastien Guerry <bzg@gnu.org>
;; SPDX-License-Identifier: EPL-2.0
;; License-Filename: LICENSES/EPL-2.0.txt

(ns bone.common
  "Shared utilities for BONE.  The invariant is: no datalevin dependency,
  loadable by both JVM and Babashka.  Mostly pure functions, plus a few
  file/env readers (load-config, load-mailmap, read-failures-file...)."
  (:require [clojure.string :as str]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.pprint :as pp]
            [clojure.walk :as walk]
            [taoensso.timbre :as log])
  (:import [java.text Normalizer Normalizer$Form]
           [java.security MessageDigest]
           [java.time LocalDate ZoneId]
           [java.time.format DateTimeFormatter]
           [java.util Date]))

;; ---------------------------------------------------------------------------
;; Constants
;; ---------------------------------------------------------------------------

(def bone-format
  "BONE export format version. Bump when the JSON/Org export shape changes."
  "0.9.5")

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
;; Command failure labels
;; ---------------------------------------------------------------------------

(def reason-labels
  "Human-readable labels for command-failure :reason keys."
  {:unknown-target     "unknown target"
   :type-mismatch      "type mismatch between source and target"
   :self-loop          "target is the same report as the source"
   :insufficient-scope "insufficient permissions"})

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

(defn escape-script-payload
  "Neutralize HTML-script-closing and comment sequences in a JSON payload that
   will be inlined inside an HTML <script> tag.  Email-controlled fields
   (subject, from-name, ...) reach this path verbatim, so a `</script>` in any
   string would otherwise break out of the script element.
   JS-literal context only, not JSON.parse-safe."
  ^String [^String s]
  (-> s
      (str/replace "</" "<\\/")
      (str/replace "<!--" "<\\!--")
      (str/replace "-->" "--\\>")))

(def patch-filename-re #"(?i)\.(patch|diff)$")

(defn patch-file?
  "True if filename looks like a patch/diff file."
  [filename]
  (boolean (and filename (re-find patch-filename-re filename))))

(def ics-filename-re #"(?i)\.ics$")

(defn ics-file?
  "True if filename looks like an ICS calendar file."
  [filename]
  (boolean (and filename (re-find ics-filename-re filename))))

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

(defn has-ics-attachment?
  "True if any attachment has a .ics filename."
  [attachments]
  (boolean (some #(ics-file? (:attachment/filename %)) attachments)))

(defn has-inline-ics?
  "True if body text contains inline ICS/VCALENDAR content."
  [body-text]
  (and (some? body-text)
       (str/includes? body-text "BEGIN:VCALENDAR")
       (str/includes? body-text "BEGIN:VEVENT")))

;; ---------------------------------------------------------------------------
;; ICS (iCalendar) parsing and assembly -- pure, shared JVM/bb.
;; ---------------------------------------------------------------------------

(defn normalize-ics-eol
  "Normalize line endings to CRLF as required by RFC 5545.  Lone LF, lone
  CR, and existing CRLF all collapse to a single CRLF.  nil-safe."
  [s]
  (when s (str/replace s #"\r\n?|\n" "\r\n")))

(defn- ics-component-re [component]
  (re-pattern (str "(?s)BEGIN:" component "(?:(?!BEGIN:" component ").)*?END:" component)))

(defn extract-ics-blocks
  "Extract BEGIN:<component>...END:<component> blocks from ICS text.  Each
  returned block is CRLF-normalized and CRLF-terminated.  The reluctant
  quantifier is bounded by the next BEGIN to avoid catastrophic
  backtracking and to keep successive components from merging."
  [component text]
  (when (and text (str/includes? text (str "BEGIN:" component)))
    (map #(str (normalize-ics-eol %) "\r\n")
         (re-seq (ics-component-re component) text))))

(defn extract-vevents
  "Extract CRLF-normalized VEVENT blocks from ICS text."
  [text]
  (extract-ics-blocks "VEVENT" text))

(defn extract-vtimezones
  "Extract CRLF-normalized VTIMEZONE blocks from ICS text."
  [text]
  (extract-ics-blocks "VTIMEZONE" text))

(defn- unfold-ics
  "Undo RFC 5545 line folding (a CRLF followed by a space or tab)."
  [s]
  (str/replace s #"\r\n[ \t]" ""))

(defn ics-property
  "Read a top-level ICS property value (ignoring any parameters) from a
  component block, or nil.  Case-insensitive on the property name."
  [block prop]
  (when block
    (let [re (re-pattern (str "(?im)^" (java.util.regex.Pattern/quote prop)
                              "(?:;[^:\r\n]*)?:(.*)$"))]
      (some-> (re-find re (unfold-ics block)) second str/trim))))

(defn vevent-uid [block] (ics-property block "UID"))

(defn vevent-recurrence-id
  "RECURRENCE-ID of a VEVENT block, or nil.  Distinguishes the override of
  a single occurrence from the recurring master that shares its UID."
  [block]
  (ics-property block "RECURRENCE-ID"))

(defn vevent-sequence
  "SEQUENCE number of a VEVENT block (0 when absent or unparseable)."
  [block]
  (or (some-> (ics-property block "SEQUENCE") parse-long) 0))

(defn dedupe-vevents
  "Deduplicate VEVENT blocks, preserving first-seen order.  Blocks with a
  UID are correlated by [UID, RECURRENCE-ID], keeping the highest SEQUENCE;
  RECURRENCE-ID is part of the key so per-occurrence overrides of a
  recurring event (same UID, distinct RECURRENCE-ID) survive rather than
  collapsing into the master.  Blocks without a UID cannot be correlated,
  so they are deduplicated only when byte-for-byte identical -- the
  normalized block text itself is the key."
  [vevents]
  (let [vkey (fn [v] (if-let [uid (vevent-uid v)]
                       [uid (vevent-recurrence-id v)]
                       v))
        best (reduce (fn [m v]
                       (let [k (vkey v)]
                         (if (and (m k) (>= (vevent-sequence (m k)) (vevent-sequence v)))
                           m
                           (assoc m k v))))
                     {} vevents)]
    ;; distinct preserves first-seen key order.
    (mapv best (distinct (map vkey vevents)))))

(defn dedupe-vtimezones
  "Keep one VTIMEZONE per TZID, first-seen.  VTIMEZONEs without a TZID are
  dropped -- a TZID is mandatory and an anonymous one is unreferenceable."
  [vtimezones]
  (->> vtimezones
       (filter #(ics-property % "TZID"))
       (reduce (fn [[acc seen] vtz]
                 (let [tzid (ics-property vtz "TZID")]
                   (if (seen tzid)
                     [acc seen]
                     [(conj acc vtz) (conj seen tzid)])))
               [[] #{}])
       first))

(defn escape-ics-text
  "Escape an ICS TEXT-typed property value per RFC 5545 section 3.3.11:
  backslash, semicolon and comma are backslash-escaped, and newlines
  become the literal `\\n`.  nil-safe."
  [s]
  (when s
    (-> s
        (str/replace "\\" "\\\\")
        (str/replace ";" "\\;")
        (str/replace "," "\\,")
        (str/replace #"\r\n?|\n" "\\\\n"))))

(defn fold-ics-line
  "Fold a single content line to <=75 octets per RFC 5545 section 3.1,
  inserting a CRLF followed by one space at each fold point.  Counts
  UTF-8 octets (not chars) and never splits a multi-octet character.
  Returns the folded line without a trailing CRLF.  nil-safe."
  [line]
  (when line
    (let [^String s line
          limit    75
          len      (.length s)]
      (loop [i 0
             width 0
             ^StringBuilder out (StringBuilder.)]
        (if (>= i len)
          (.toString out)
          ;; Iterate by code point, not by UTF-16 char: a non-BMP
          ;; character (e.g. an emoji) is a surrogate pair whose two
          ;; halves must never be separated by a fold, and whose UTF-8
          ;; size (4 octets) must be counted as a whole.
          (let [c  (.charAt s i)
                cp (if (and (Character/isHighSurrogate c)
                            (< (inc i) len)
                            (Character/isLowSurrogate (.charAt s (inc i))))
                     (str c (.charAt s (inc i)))
                     (str c))
                n  (alength (.getBytes cp "UTF-8"))
                i' (+ i (.length ^String cp))]
            (if (> (+ width n) limit)
              (recur i' (+ 1 n) (-> out (.append "\r\n ") (.append cp)))
              (recur i' (+ width n) (.append out cp)))))))))

(defn build-vcalendar
  "Assemble a complete VCALENDAR document (CRLF-terminated) wrapping
  the given VTIMEZONE and VEVENT blocks; nil when no event.  Always
  METHOD:PUBLISH (read-only subscription feed): any iTIP METHOD from
  the source is dropped, but STATUS:CANCELLED inside a block is
  preserved verbatim."
  [cal-name vevents vtimezones]
  (when (seq vevents)
    (str "BEGIN:VCALENDAR\r\n"
         "VERSION:2.0\r\n"
         "PRODID:-//BONE//Event Export//EN\r\n"
         "METHOD:PUBLISH\r\n"
         (fold-ics-line (str "X-WR-CALNAME:" (escape-ics-text cal-name))) "\r\n"
         (str/join "" vtimezones)
         (str/join "" vevents)
         "END:VCALENDAR\r\n")))

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
    (->> (str/split-lines body)
         (take-while #(not (re-find patch-start-line-re %)))
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

;; Thread-safe formatters (DateTimeFormatter is immutable and safe to share).
(def ^:private datetime-fmt
  (-> (DateTimeFormatter/ofPattern "yyyy-MM-dd HH:mm")
      (.withZone (ZoneId/of "UTC"))))

(def ^:private date-iso-fmt
  (-> (DateTimeFormatter/ofPattern "yyyy-MM-dd")
      (.withZone (ZoneId/of "UTC"))))

(defn format-date
  "Format a java.util.Date as 'yyyy-MM-dd HH:mm' in UTC."
  [^Date date]
  (when date
    (.format datetime-fmt (.toInstant date))))

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

(defn parse-cutoff-date
  "Resolve a retention value to a java.util.Date cutoff in the past.
  Accepts an ISO date (\"2024-01-01\") or a duration string (\"90d\", \"6m\")
  relative to now. Returns nil when the value is not a string or unparseable."
  [v]
  (when (string? v)
    (or (parse-iso-date v)
        (when-let [days (parse-delay v)]
          (Date. (- (System/currentTimeMillis)
                    (* (long days) 24 60 60 1000)))))))

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

(defn extract-list-id
  "Extract the identifier from a List-Id header value.
  RFC 2919: \"Description <list-id>\" -> \"list-id\"."
  [raw]
  (when raw
    (if-let [[_ id] (re-find #"<([^>]+)>" (str raw))]
      id
      (str raw))))

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

(defn- bracketed-token
  "First <addr@domain> token in v (string or vector); nil otherwise."
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

(defn extract-bracketed-id-raw
  "Like extract-bracketed-id but preserves the original case of the
  id-right.  Used at export time for archive URLs (e.g. public-inbox)
  that compare message-ids case-sensitively."
  [v]
  (bracketed-token v))

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

(defn original-recipient
  "Extract the original recipient address from MTA headers.
  Checks X-Original-To, Envelope-To, X-Envelope-To in order."
  [headers-edn]
  (or (get-header headers-edn "X-Original-To")
      (get-header headers-edn "Envelope-To")
      (get-header headers-edn "X-Envelope-To")))

(defn- multiple-delivered-to?
  "True when Delivered-To contains 2+ distinct addresses (alias signal)."
  [headers-edn]
  (let [vals (->> (get-header-values headers-edn "Delivered-To")
                  (map str/lower-case)
                  distinct)]
    (> (count vals) 1)))

(defn- original-recipient-not-in-to-cc?
  "True when the original recipient address appears in neither To nor Cc.
  Repeated headers come back from get-header as vectors: take the first
  original recipient, and match against all To/Cc values joined."
  [hdrs]
  (let [joined (fn [header-name]
                 (when-let [vals (seq (get-header-values hdrs header-name))]
                   (str/lower-case (str/join " " vals))))
        orig (some-> (original-recipient hdrs)
                     (as-> v (if (vector? v) (first v) v))
                     str/lower-case)
        to   (joined "To")
        cc   (joined "Cc")]
    (boolean (and orig
                  (not (some-> to (str/includes? orig)))
                  (not (some-> cc (str/includes? orig)))))))

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

(defn- matches?
  "Case-insensitive substring match."
  [haystack needle]
  (and haystack needle
       (str/includes? (str/lower-case (str haystack))
                      (str/lower-case needle))))

(defn- any-matches?
  "True if any value in coll matches needle (case-insensitive substring)."
  [coll needle]
  (boolean (some #(matches? % needle) coll)))

(defn- match-source?
  "Check if headers match a source based on its inferred type."
  [headers-edn source]
  (case (source-type source)
    :mailing-list (matches? (extract-list-id (get-header headers-edn "List-Id"))
                            (:list source))
    :alias        (or (matches? (original-recipient headers-edn)
                                (:alias source))
                      (any-matches? (get-header-values headers-edn "Delivered-To")
                                    (:alias source)))
    :mailbox      (any-matches? (get-header-values headers-edn "Delivered-To")
                                (:to source))
    false))

(defn classify-source
  "Return the :name of the first matching source, or nil.
  Uses header-based matching only (List-Id, X-Original-To, Delivered-To)."
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

(def report-type-keywords (into #{} (map :type) report-type-spec))

(def type->plural (into {} (map (juxt :type :plural)) report-type-spec))

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
;; Reproducible per-source config (published in docs.html + reports/config.edn)
;; ---------------------------------------------------------------------------
;;
;; Lets anyone re-run BONE on their own copy of the mail and get the
;; same dashboard: the source's rules with global :labels/:commands/...
;; folded in, minus everything secret or operator-internal
;; (:mailboxes, :db, :logging, :notifications).

(def reproducible-config-header
  (str ";; Minimal config.edn to reproduce this dashboard on your own copy of\n"
       ";; the mail.  Replace :mailboxes with YOUR local source -- a Maildir of\n"
       ";; your subscription, or your own IMAP account; the operator's mailbox\n"
       ";; is private and not needed here.  See \"Deploying BONE\" in the manual,\n"
       ";; then validate with: bb test-config\n"))

(defn- merge-command-entry
  "Merge one command's global and local config the way the daemon
  resolves them: the :words vector and the {:scope :report-types}
  override unit are each replaced wholesale by the local entry when it
  defines them (see merged-raw-words and resolve-command-overrides) --
  never deep-merged, so a local {:scope ...} does not inherit the
  global entry's :report-types."
  [g l]
  (let [words     (if (contains? l :words)
                    (select-keys l [:words])
                    (select-keys g [:words]))
        overrides (let [lo (select-keys l [:scope :report-types])]
                    (if (seq lo) lo (select-keys g [:scope :report-types])))]
    (merge words overrides)))

(defn effective-source-config
  "Pure.  Return a sanitised, self-contained source map for `source-name`:
  per-source values with the operator's global :labels/:commands/... folded
  in, so it reproduces their interpretation without their global config.
  Drops secrets and operator-internal keys (notably :notifications).
  Returns nil when no source matches."
  [config source-name]
  (when-let [src (some #(when (= (:name %) source-name) %) (:sources config))]
    (let [;; Emit exactly the one matcher source-type resolves (list >
          ;; alias > to), so the published config is valid (the schema
          ;; forbids several) and faithful to what the daemon actually
          ;; matches on.
          matcher  (cond (:list src)  [:list  (:list src)]
                         (:alias src) [:alias (:alias src)]
                         (:to src)    [:to    (:to src)])
          labels   (merge (:labels config) (:labels src))
          commands (merge-with merge-command-entry
                               (:commands config) (:commands src))
          csyntax  (or (:command-syntax src) (:command-syntax config))
          rtypes   (or (:report-types src) (:report-types config))
          restr    (or (:restricted-types src) (:restricted-types config))
          expiry   (or (:expiry src) (:expiry config))
          awaiting (or (:awaiting-delay src) (:awaiting-delay config))
          formats  (or (:export-formats src) (:export-formats config))
          ptrig    (cond (contains? src    :patch-triggers?) (:patch-triggers? src)
                         (contains? config :patch-triggers?) (:patch-triggers? config)
                         :else true)]
      (apply array-map
             (mapcat identity
                     (cond-> [[:name (:name src)]]
                       matcher                      (conj matcher)
                       (:base-url src)              (conj [:base-url (:base-url src)])
                       (:list-archive src)          (conj [:list-archive (:list-archive src)])
                       (:archive-format-string src) (conj [:archive-format-string (:archive-format-string src)])
                       (seq (:maintainers src))     (conj [:maintainers (:maintainers src)])
                       (seq labels)                 (conj [:labels labels])
                       (seq commands)               (conj [:commands commands])
                       (= :strict csyntax)          (conj [:command-syntax :strict])
                       rtypes                       (conj [:report-types rtypes])
                       restr                        (conj [:restricted-types restr])
                       (seq expiry)                 (conj [:expiry expiry])
                       awaiting                     (conj [:awaiting-delay awaiting])
                       (false? ptrig)               (conj [:patch-triggers? false])
                       formats                      (conj [:export-formats formats])
                       (seq (:periods src))         (conj [:periods (:periods src)])))))))

(defn reproducible-config
  "Pure.  EDN data reproducing `source-name`: a complete config whose
  :mailboxes is a placeholder the reader replaces with their own local
  source.  Returns nil when no source matches."
  [config source-name]
  (when-let [src (effective-source-config config source-name)]
    (array-map
     :mailboxes [(array-map :name "local" :type :maildir
                            :path "/path/to/your/Maildir" :folder "")]
     :sources   [src])))

(defn reproducible-config-str
  "Pure.  `reproducible-config` rendered as a commented, pretty-printed
  config.edn string, or nil when no source matches."
  [config source-name]
  (when-let [data (reproducible-config config source-name)]
    (str reproducible-config-header
         (str/trimr (with-out-str (pp/pprint data)))
         "\n")))

;; ---------------------------------------------------------------------------
;; Maintainer tenures (pure -- operate on a seq of tenure maps, no DB access)
;; ---------------------------------------------------------------------------
;;
;; A "tenure" is a map {:email str :from Date|nil :to Date|nil :order long|nil}
;; representing one contiguous period during which an address held maintainer
;; status on a source. :from nil = active since the beginning of time.
;; :to nil = the tenure is currently active. :order encodes the config order
;; used as a tie-break when computing the lead maintainer.

(defn active-tenures
  "Return tenures that are currently active (no :to)."
  [tenures]
  (filter #(nil? (:to %)) tenures))

(defn- tenure-sort-key [t]
  [(if-let [^Date f (:from t)] (.getTime f) 0)
   (or (:order t) Long/MAX_VALUE)
   (str/lower-case (or (:email t) ""))])

(defn lead-maintainer
  "Return the lower-cased email of the lead maintainer -- the active tenure
  with the earliest :from (nil sorts first), tie-broken by :order then email.
  Returns nil if no active tenure."
  [tenures]
  (:email (first (sort-by tenure-sort-key (active-tenures tenures)))))

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
;; Config and source-map
;; ---------------------------------------------------------------------------

(def config-edn-readers
  "EDN reader tags accepted in `config.edn`.

   `#bone/env \"VAR\"` resolves to the named environment variable.
   Raises if the variable is unset -- prefer a clear startup failure
   over silently using a nil password."
  {'bone/env (fn [var-name]
               (let [k (str var-name)]
                 (or (System/getenv k)
                     (throw (ex-info (str "Environment variable not set: " k)
                                     {:var k})))))})

(defn- resolve-password-files
  "Replace every `{:password-file path}` in the config with the trimmed
   contents of the file.  Applied across :mailboxes and :notifications
   :smtp.  Raises if both `:password` and `:password-file` are set, or
   if the referenced file is missing."
  [config]
  (walk/postwalk
   (fn [x]
     (if-let [pf (and (map? x) (:password-file x))]
       (do
         (when (:password x)
           (throw (ex-info ":password and :password-file are mutually exclusive"
                           {:offending x})))
         (let [f (io/file (expand-home pf))]
           (when-not (.exists f)
             (throw (ex-info (str ":password-file not found: " pf)
                             {:path pf})))
           (-> x (dissoc :password-file)
               (assoc :password (str/trim (slurp f))))))
       x))
   config))

(defn load-config
  "Load config.edn if it exists, or nil.  With no args, consults the
  BONE_CONFIG env var, falling back to ./config.edn -- so all bb
  scripts honor a single override point without per-script flags.

  Resolves `#bone/env \"VAR\"` reader tags and `:password-file` entries
  before returning, so callers see a fully-materialised config."
  ([] (load-config (or (System/getenv "BONE_CONFIG") "config.edn")))
  ([path]
   (let [f (io/file path)]
     (when (.exists f)
       (-> (edn/read-string {:readers config-edn-readers} (slurp f))
           resolve-password-files)))))

(defn inline-password-locations
  "Read config.edn as it was written -- without resolving #bone/env or
   :password-file -- and return a vector of human-readable location
   labels where :password is a plain inline string.  Used by
   `bb test-config` to remind operators that credentials can live in
   a sidecar file or environment variable instead."
  [path]
  (let [f (io/file path)]
    (when (.exists f)
      (let [env-marker ::env
            raw        (edn/read-string
                        {:readers {'bone/env (constantly env-marker)}}
                        (slurp f))
            inline?    #(and (map? %)
                             (string? (:password %))
                             (not (:password-file %)))
            mailboxes  (for [mb (:mailboxes raw) :when (inline? mb)]
                         (str "mailbox " (pr-str (:name mb))))
            smtp       (when (inline? (get-in raw [:notifications :smtp]))
                         "notifications :smtp")]
        (vec (cond-> (vec mailboxes) smtp (conj smtp)))))))

(defn load-mailmap
  "Load ./mailmap.edn (shape `{\"Canonical Name\" [emails...]}`) and return
  `{email-lc -> canonical-name}` (inverted, flat).  Returns `{}` if
  the file is absent.  Export-only -- never read on the ingest path."
  ([] (load-mailmap "mailmap.edn"))
  ([path]
   (let [f (io/file path)]
     (if (.exists f)
       (let [m (edn/read-string (slurp f))]
         (reduce-kv
          (fn [acc nm emails]
            (reduce (fn [a e] (assoc a (str/lower-case e) nm))
                    acc
                    emails))
          {} m))
       {}))))

(defn db-path
  "Resolve the Datalevin DB path: prefer :db {:path ...} from the
  config, fall back to BONE_DB env var, then default \"data/bone-db\"."
  [config]
  (or (get-in config [:db :path])
      (System/getenv "BONE_DB")
      "data/bone-db"))

(defn build-source-map
  "Build source-name -> config map from config."
  [config]
  (let [global-st       (:labels config)
        global-cmd      (:commands config)
        global-ef       (:export-formats config)
        global-expiry   (:expiry config)
        global-rt       (:report-types config)
        global-cs       (:command-syntax config)
        global-restr    (:restricted-types config)
        global-ptrig?   (contains? config :patch-triggers?)
        global-ptrig    (:patch-triggers? config)]
    (into {}
          (keep (fn [src]
                 (if-let [stype (source-type src)]
                   (let [restricted (if (contains? src :restricted-types)
                                      (:restricted-types src)
                                      global-restr)
                         ptrig-set? (or (contains? src :patch-triggers?)
                                        global-ptrig?)
                         ptrig      (if (contains? src :patch-triggers?)
                                      (:patch-triggers? src)
                                      global-ptrig)]
                     [(:name src)
                      (merge {:source-type stype}
                             (select-keys src [:list :alias :to :commands :labels :notifications
                                               :archive-format-string :list-archive :base-url
                                               :website :contribute-url :post-address
                                               :maintainers :awaiting-delay :periods])
                             (when global-st {:global-labels global-st})
                             (when global-cmd {:global-commands global-cmd})
                             (cond-> {:export-formats (set (or (:export-formats src) global-ef ["json" "org" "rss"]))
                                      :report-types (when-let [rt (or (:report-types src) global-rt)]
                                                      (set (map keyword rt)))
                                      :expiry (or (:expiry src) global-expiry)
                                      :command-syntax (or (:command-syntax src) global-cs :loose)}
                               (some? restricted)
                               (assoc :restricted-types (set (map keyword restricted)))

                               ptrig-set?
                               (assoc :patch-triggers? (boolean ptrig))))])
                   (log/warn "Source has no :list, :alias, or :to key -- skipping:" (:name src)))))
          (:sources config))))

;; ---------------------------------------------------------------------------
;; Report scoring
;; ---------------------------------------------------------------------------

(defn report-priority [report]
  (+ (if (:report/urgent report) 2 0)
     (if (:report/important report) 1 0)))

(defn report-status [report]
  (+ (if-not (:report/closed report) 4 0)
     (if (:report/owned report) 2 0)
     (if (:report/acked report) 1 0)))

(defn report-descendant-count [report]
  (let [d (:report/descendants report)]
    (if (coll? d) (count d) 0)))

;; ---------------------------------------------------------------------------
;; Canonical report pull pattern
;; ---------------------------------------------------------------------------

(def report-pull-pattern
  ;; Each :report/<state> ref pulls its email's author + date so
  ;; consumers can show "set by X on Y" without a second query.
  ;; Relations come via :rel/_from and :rel/_to (both directions are
  ;; stored for asymmetric kinds, consumer picks the relevant side).
  '[:db/id :report/type :report/version
    :report/patch-seq :report/patch-source :report/message-id
    {:report/acked [:email/author-address :email/date-sent]}
    {:report/owned [:email/author-address :email/date-sent]}
    {:report/closed [:email/author-address :email/date-sent]}
    {:report/urgent [:email/author-address :email/date-sent]}
    {:report/important [:email/author-address :email/date-sent]}
    :report/acked-address :report/owned-address :report/closed-address
    :report/urgent-address :report/important-address
    :report/close-reason
    {:report/topic [:email/author-address :email/date-sent]}
    :report/topic-value
    {:report/deadline [:email/author-address :email/date-sent]}
    :report/deadline-value
    {:report/expiry [:email/author-address :email/date-sent]}
    :report/expiry-value
    :report/has-ics :report/has-text-attachments
    :report/trailers
    :report/last-activity :report/last-activity-address :report/descendants :report/updated-at
    ;; :email/source lets the export resolve the linked report's own
    ;; source (archive-url suppression and format-string).
    {:rel/_from [:rel/kind :rel/active? :rel/setter :rel/posed-at :rel/value
                 {:rel/to [:db/id :report/type :report/message-id
                           {:report/email [:email/subject :email/headers-edn
                                           :email/source]}]}]}
    {:rel/_to [:rel/kind :rel/active? :rel/setter :rel/posed-at :rel/value
               {:rel/from [:db/id :report/type :report/message-id
                           {:report/email [:email/subject :email/headers-edn
                                           :email/source]}]}]}
    {:report/series [:series/id :series/expected :series/closed
                     {:series/patches [:db/id]}
                     {:series/cover-letter [:email/message-id]}]}
    ;; :patch/text is deliberately excluded -- patch diffs are the
    ;; bulkiest text in the DB.  dump-patches! pulls it on demand, only
    ;; for the reports it actually writes.
    {:report/patches [:patch/filename :patch/source
                      :patch/author :patch/subject :patch/date]}
    {:report/email [:email/subject :email/author-address :email/author-name
                    :email/from-address :email/from-name
                    :email/date-sent :email/source :email/id
                    :email/headers-edn]}])

;; Pull pattern for attachment-heavy operations (event/text export).
;; Used by bb scripts to fetch attachment data only for reports that need it.
(def attachment-pull-pattern
  '[:db/id :report/type :report/message-id
    {:report/email [:email/body-text
                    {:email/attachments [:attachment/filename :attachment/content-type
                                         :attachment/size :attachment/data]}]}])

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

;; ---------------------------------------------------------------------------
;; Vote helpers (pure -- no datalevin dependency)
;; ---------------------------------------------------------------------------

(defn votes-by-report
  "Group raw vote tuples into {report-eid [{:value :up :voter \"a@b\"
  :email-mid \"<...>\" :email-mid-raw \"<...>\"} ...]}.
  :email-mid-raw preserves the original case (defaults to :email-mid
  when headers-edn is absent).  Expects tuples of [report-eid value
  voter email-mid headers-edn?] (headers-edn optional)."
  [vote-tuples]
  (reduce (fn [acc tuple]
            (let [[r val voter emid hdrs] tuple
                  raw (when hdrs (extract-bracketed-id-raw
                                  (get-header hdrs "Message-Id")))]
              (update acc r (fnil conj [])
                      {:value val :voter voter
                       :email-mid emid :email-mid-raw (or raw emid)})))
          {}
          vote-tuples))

(defn vote-counts
  "Derive {:up n :down n :null n} from a seq of vote maps."
  [votes]
  (reduce (fn [acc {:keys [value]}]
            (update acc value (fnil inc 0)))
          {:up 0 :down 0 :null 0}
          votes))

;; ---------------------------------------------------------------------------
;; CLI arg parsing
;; ---------------------------------------------------------------------------

(defn- flag-like?
  "True if token starts with '-' (looks like a flag rather than a value)."
  [s]
  (and s (str/starts-with? s "-")))

(defn- check-flag-value
  "Validate that flag `a` has a proper value `v`.
  Returns :ok, :missing, or :flag-as-value."
  [a v]
  (cond
    (nil? v)       (do (log/warn "Flag" a "requires a value") :missing)
    (flag-like? v) (do (log/warn "Flag" a "followed by flag" v "-- missing value?") :flag-as-value)
    :else          :ok))

(def ^:private valued-flags
  "Map of flag names to [opt-key transform-fn]. Flags that share
  short and long forms map both names to the same entry."
  {"-o" [:out-file identity] "--output" [:out-file identity]
   "-n" [:source-name identity] "--source" [:source-name identity]
   "--json" [:json-file identity] "--dir" [:out-dir identity]
   "--html-theme" [:theme identity]
   "-p" [:min-priority parse-long] "--min-priority" [:min-priority parse-long]
   "-s" [:min-status parse-long] "--min-status" [:min-status parse-long]
   "--html-page-size" [:page-size parse-long]
   "--html-columns" [:html-columns identity]
   "--html-columns-sort" [:html-columns-sort identity]
   "--closed-retention" [:closed-retention identity]
   "--topics-filter" [:topics-filter identity]})

(defn parse-cli-args
  "Parse common CLI flags into a map.
  Recognises: -o/--output, -n/--source, -p/--min-priority, -s/--min-status,
  --json, --dir, --force, --html-theme, --html-page-size, --html-columns,
  --html-columns-sort, --closed-retention, --topics-filter.
  Any leading non-flag token is captured as :format.
  Warns when a valued flag is missing its argument or followed by another flag."
  [args]
  (loop [opts {} [a & [v & r :as more]] args]
    (cond
      (nil? a)                        opts
      (= "--force" a)                 (recur (assoc opts :force-all? true) more)
      (= "--index-only" a)            (recur (assoc opts :format "root") more)
      (contains? valued-flags a)      (let [[k xf] (valued-flags a)]
                                        (case (check-flag-value a v)
                                          :ok            (recur (assoc opts k (xf v)) r)
                                          :flag-as-value (recur opts more)
                                          :missing       opts))
      (not (:format opts))            (recur (assoc opts :format a) more)
      :else                           (do (when (flag-like? a)
                                                (log/debug "Ignoring unrecognized flag:" a))
                                          (recur opts more)))))
