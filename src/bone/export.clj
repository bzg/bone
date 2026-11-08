;; Copyright (c) 2026 Bastien Guerry <bzg@gnu.org>
;; SPDX-License-Identifier: EPL-2.0
;; License-Filename: LICENSES/EPL-2.0.txt

(ns bone.export
  "What the read-only Babashka tier -- export, index, stats, docs,
  notify, maintenance -- needs and the JVM daemon does not: the export
  format version, the report pull patterns, report scoring and labels,
  the reproducible per-source config, vote grouping, mailmap loading
  and CLI parsing.  Pure functions but for the mailmap reader, the
  clock in `parse-cutoff-date` and the warnings of `parse-cli-args`;
  no datalevin dependency: loadable by both tiers like `bone.common`,
  which it builds on."
  (:require [bone.common :as common]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.pprint :as pp]
            [clojure.string :as str]
            [taoensso.timbre :as log])
  (:import [java.time ZoneId]
           [java.time.format DateTimeFormatter]
           [java.util Date]))

;; ---------------------------------------------------------------------------
;; Constants and labels
;; ---------------------------------------------------------------------------

(def bone-format
  "BONE export format version. Bump when the JSON/Org export shape changes."
  "0.9.5")

(def reason-labels
  "Human-readable labels for command-failure :reason keys."
  {:unknown-target     "unknown target"
   :type-mismatch      "type mismatch between source and target"
   :self-loop          "target is the same report as the source"
   :insufficient-scope "insufficient permissions"})

(def type->plural
  "Report type keyword => the noun used in exports."
  (into {} (map (juxt :type :plural)) common/report-type-spec))

;; ---------------------------------------------------------------------------
;; Dates
;; ---------------------------------------------------------------------------

;; Thread-safe formatter (DateTimeFormatter is immutable and safe to share).
(def ^:private datetime-fmt
  (-> (DateTimeFormatter/ofPattern "yyyy-MM-dd HH:mm")
      (.withZone (ZoneId/of "UTC"))))

(defn format-date
  "Format a java.util.Date as 'yyyy-MM-dd HH:mm' in UTC."
  [^Date date]
  (when date
    (.format datetime-fmt (.toInstant date))))

(defn parse-cutoff-date
  "Resolve a retention value to a java.util.Date cutoff in the past.
  Accepts an ISO date (\"2024-01-01\") or a duration string (\"90d\", \"6m\")
  relative to now. Returns nil when the value is not a string or unparseable."
  [v]
  (when (string? v)
    (or (common/parse-iso-date v)
        (when-let [days (common/parse-delay v)]
          (Date. (- (System/currentTimeMillis)
                    (* (long days) 24 60 60 1000)))))))

;; ---------------------------------------------------------------------------
;; HTML
;; ---------------------------------------------------------------------------

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

;; ---------------------------------------------------------------------------
;; Message-ids
;; ---------------------------------------------------------------------------

(defn extract-bracketed-id-raw
  "Like `common/extract-bracketed-id` but preserves the original case
  of the id-right, for archive URLs (e.g. public-inbox) that compare
  message-ids case-sensitively."
  [v]
  (common/bracketed-token v))

;; ---------------------------------------------------------------------------
;; Reproducible per-source config (published in docs.html + reports/config.edn)
;; ---------------------------------------------------------------------------
;;
;; Lets anyone re-run BONE on their own copy of the mail and get the
;; same dashboard: the source's rules with global :labels/:commands/...
;; folded in, minus everything secret or operator-internal
;; (:mailboxes, :db, :logging, :notifications).

(defn- merge-command-entry
  "Merge one command's global and local config the way the daemon
  resolves them: the :words vector and the {:scope :report-types}
  override unit are each replaced wholesale by the local entry when it
  defines them (see `common/resolve-commands-map` and
  `common/resolve-command-overrides`) -- never deep-merged, so a local
  {:scope ...} does not inherit the global entry's :report-types."
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

(def ^:private reproducible-config-header
  (str ";; Minimal config.edn to reproduce this dashboard on your own copy of\n"
       ";; the mail.  Replace :mailboxes with YOUR local source -- a Maildir of\n"
       ";; your subscription, or your own IMAP account; the operator's mailbox\n"
       ";; is private and not needed here.  See \"Deploying BONE\" in the manual,\n"
       ";; then validate with: bb test-config\n"))

(defn reproducible-config-str
  "Pure.  `reproducible-config` rendered as a commented, pretty-printed
  config.edn string, or nil when no source matches."
  [config source-name]
  (when-let [data (reproducible-config config source-name)]
    (str reproducible-config-header
         (str/trimr (with-out-str (pp/pprint data)))
         "\n")))

;; ---------------------------------------------------------------------------
;; Report scoring
;; ---------------------------------------------------------------------------

(defn report-status [report]
  (+ (if-not (:report/closed report) 4 0)
     (if (:report/owned report) 2 0)
     (if (:report/acked report) 1 0)))

(defn report-descendant-count [report]
  (let [d (:report/descendants report)]
    (if (coll? d) (count d) 0)))

;; ---------------------------------------------------------------------------
;; Report pull patterns
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

;; Pull pattern for attachment-heavy operations (event/text export):
;; attachment data only for the reports that need it.
(def attachment-pull-pattern
  '[:db/id :report/type :report/message-id
    {:report/email [:email/body-text
                    {:email/attachments [:attachment/filename :attachment/content-type
                                         :attachment/size :attachment/data]}]}])

;; ---------------------------------------------------------------------------
;; Votes
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
                                  (common/get-header hdrs "Message-Id")))]
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
;; Mailmap
;; ---------------------------------------------------------------------------

(defn load-mailmap
  "Load ./mailmap.edn (shape `{\"Canonical Name\" [emails...]}`) and return
  `{email-lc -> canonical-name}` (inverted, flat).  Returns `{}` if
  the file is absent.  Never read on the ingest path."
  []
  (let [f (io/file "mailmap.edn")]
    (if (.exists f)
      (let [m (edn/read-string (slurp f))]
        (reduce-kv
         (fn [acc nm emails]
           (reduce (fn [a e] (assoc a (str/lower-case e) nm))
                   acc
                   emails))
         {} m))
      {})))

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
  --json, --dir, --force, --index-only, --html-theme, --html-page-size, --html-columns,
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
