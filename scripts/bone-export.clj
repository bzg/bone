#!/usr/bin/env bb

;; bone-export.clj -- Export BONE reports per source.
;;
;; Each source gets its own directory tree under public/:
;;   public/<source-name>/index.html
;;   public/<source-name>/data.html
;;   public/<source-name>/docs.html
;;   public/<source-name>/reports/all.json
;;   public/<source-name>/reports/all.xml
;;   public/<source-name>/reports/all.org
;;   public/<source-name>/reports/bugs.json  (etc.)
;;   public/<source-name>/reports/stats.json
;;   public/<source-name>/events/<mid-hash>/<file>.ics
;;   public/<source-name>/events/announcements.ics
;;   public/<source-name>/events/announcements-open.ics
;;   public/<source-name>/events/announcements-closed.ics
;;   public/<source-name>/reports/events.json
;;   public/<source-name>/reports/events.org
;;   public/<source-name>/reports/events-closed.json
;;   public/<source-name>/reports/events-closed.org
;;   public/<source-name>/text/<mid-hash>/<file>
;;   public/<source-name>/patches/<mid-hash>/<file>
;;
;; Usage:
;;   bb export               -- incremental export (skip if nothing changed)
;;   bb export json          -- export all.json for each source
;;   bb export rss           -- export all.xml for each source
;;   bb export org           -- export all.org for each source
;;   bb export html          -- generate index.html for each source
;;   bb export stats         -- generate stats.json for each source
;;   bb export patches       -- export patch files for each source
;;   bb export events        -- export ICS event files and events.ics for each source
;;   bb export text          -- export text/plain and text/x-log attachments
;;   bb export root          -- regenerate public/index.html (source listing)
;;   bb export all           -- all formats (still incremental)
;;   bb export --force       -- force full export, ignore timestamps
;;   bb export json -n src   -- export only source "src"
;;   bb export json -p 2     -- only priority >= 2
;;   bb export json -s 3     -- only status >= 3
;;
;; Environment / defaults:
;;   BONE_DB -- path to db (default: ./data/bone-db)

(ns bone-export
  (:require [babashka.process :as process]
            [cheshire.core :as json]
            [clojure.string :as str]
            [clojure.java.io :as io]
            [taoensso.timbre :as log]
            [bone.common :refer [parse-headers slugify mid-hash
                                 format-date format-date-iso
                                 extract-bracketed-id-raw
                                 report-priority report-status report-descendant-count
                                 parse-cli-args parse-delay parse-cutoff-date
                                 load-config load-mailmap db-path build-source-map
                                 reproducible-config-str
                                 bone-schema bone-format
                                 report-type-spec type->plural
                                 votes-by-report vote-counts
                                 ics-file? text-attachment? email-body-text
                                 normalize-ics-eol
                                 extract-vevents extract-vtimezones
                                 dedupe-vevents dedupe-vtimezones build-vcalendar]]
            [bone.common-bb :refer [load-datalevin-pod! all-reports dq dpull
                                    fetch-attachment-data get-tenures tenures-snapshot
                                    get-last-modified changed-source-types-since
                                    new-source-types-since
                                    state-changed-source-types-since]]
            [bone.html-bb :refer [set-theme! html-head footer-css bone-footer
                                  wrap-js spit-html theme-toggle-js nav-bar]]
            [hiccup2.core :as h]))

(load-datalevin-pod!)

;; ---------------------------------------------------------------------------
;; File-based export timestamp (replaces DB-based save-last-export!)
;; The export is fully read-only w.r.t. the database.
;; ---------------------------------------------------------------------------

(def ^:private last-export-file "data/.last-export")

(defn- get-last-export
  "Read the last export timestamp from data/.last-export, or nil.
  A corrupt file forces a full export on the next run -- log so the
  operator knows why."
  []
  (let [f (io/file last-export-file)]
    (when (.exists f)
      (try (java.util.Date. ^long (parse-long (str/trim (slurp f))))
           (catch Exception e
             (log/warn "Could not parse" last-export-file "-- forcing full export."
                       (.getMessage e))
             nil)))))

(defn- save-last-export!
  "Write the export timestamp to data/.last-export."
  [^java.util.Date ts]
  (io/make-parents last-export-file)
  (spit last-export-file (str (.getTime ts))))

;; ---------------------------------------------------------------------------
;; Shell regeneration triggers
;; ---------------------------------------------------------------------------
;; index.html and docs.html are data-independent shells: index.html fetches
;; reports from all-open.json at runtime, and docs.html is built from the
;; template + config + maintainers.  On an incremental, data-only run they
;; are NOT rewritten (so they stay byte-stable and the cron stays quiet).
;; They ARE rebuilt when their inputs change, detected below.

;; Files whose content shapes the HTML shells.  When any is newer than the
;; last export, the shells are rebuilt even on an incremental run --
;; otherwise a code/template/asset change would not propagate until the
;; next full (`--force`) export.
(def ^:private shell-asset-files
  ["resources/bone-index.js"
   "resources/bone-search.js"
   "resources/bone-stats.js"
   "resources/bone-theme.js"
   "resources/bone-data.css"
   "resources/docs-tpl.org"
   "resources/data.org"
   "scripts/bone-index.clj"
   "scripts/bone-docs.clj"
   "scripts/bone-stats.clj"
   "scripts/bone/html_bb.clj"])

(defn- shell-assets-changed?
  "True when any shell-asset file is newer than `last-export`."
  [^java.util.Date last-export]
  (boolean
   (when last-export
     (let [le (.getTime last-export)]
       (some (fn [p]
               (let [f (io/file p)]
                 (and (.exists f) (> (.lastModified f) le))))
             shell-asset-files)))))

(defn- maintainers-changed-since?
  "True when a maintainer tenure for `source-name` opened or closed after
  `since`.  docs.html lists maintainers, so such a change must rebuild it
  even though no report's :report/updated-at moved."
  [db source-name ^java.util.Date since]
  (boolean
   (when since
     (let [st    (.getTime since)
           froms (d/q '[:find [?t ...] :in $ ?src
                        :where [?e :maint-tenure/source ?src] [?e :maint-tenure/from ?t]]
                      db source-name)
           tos   (d/q '[:find [?t ...] :in $ ?src
                        :where [?e :maint-tenure/source ?src] [?e :maint-tenure/to ?t]]
                      db source-name)]
       (some (fn [^java.util.Date dt] (> (.getTime dt) st)) (concat froms tos))))))

(defn- preserve-shell!
  "When `regen?` is false, copy a previously-exported top-level file from
  `final-dir` into `staging` so it survives the atomic swap.  Staging is
  built fresh and the subdir seed does not cover top-level files, so a
  shell we choose not to regenerate would otherwise be lost."
  [regen? final-dir staging filename]
  (when-not regen?
    (let [src (io/file final-dir filename)]
      (when (.exists src)
        (let [dst (io/file staging filename)]
          (io/make-parents dst)
          (io/copy src dst))))))

(defn- fmt-type-counts
  "Format a {report-type count} map as \"bug×3 patch×1\", sorted by type."
  [type-counts]
  (str/join " " (map (fn [[t c]] (str (name t) "×" c))
                     (sort-by (comp name key) type-counts))))

(defn- fmt-change-summary
  "Format a source's change counts for the cron line, telling genuine
  additions from effective modifications: \"new: patch×1; updated: bug×2\".
  `new-counts` are newly created reports; `state-counts` are reports whose
  own state changed (the new ones are subtracted out so each report is
  listed once).  Returns nil when nothing new or modified -- the caller
  then prints the bare source name (re-export was triggered by thread
  growth alone)."
  [new-counts state-counts]
  (let [updated (reduce-kv (fn [m t c]
                             (let [u (- c (get new-counts t 0))]
                               (cond-> m (pos? u) (assoc t u))))
                           {} (or state-counts {}))
        parts   (cond-> []
                  (seq new-counts) (conj (str "new: " (fmt-type-counts new-counts)))
                  (seq updated)    (conj (str "updated: " (fmt-type-counts updated))))]
    (when (seq parts) (str/join "; " parts))))

;; ---------------------------------------------------------------------------
;; Atomic export via staging directory
;; ---------------------------------------------------------------------------

(defn- delete-dir!
  "Recursively delete a directory and its contents."
  [^java.io.File dir]
  (when (.exists dir)
    (doseq [f (reverse (file-seq dir))]
      (.delete ^java.io.File f))))

(defn- move-dir!
  "Move `src` to `dst` using Files/move. Tries ATOMIC_MOVE first,
  falls back to a plain move if the filesystem does not support it."
  [^java.io.File src ^java.io.File dst]
  (try
    (java.nio.file.Files/move (.toPath src) (.toPath dst)
                              (into-array java.nio.file.CopyOption
                                          [java.nio.file.StandardCopyOption/ATOMIC_MOVE]))
    (catch Exception _
      (java.nio.file.Files/move (.toPath src) (.toPath dst)
                                (into-array java.nio.file.CopyOption
                                            [java.nio.file.StandardCopyOption/REPLACE_EXISTING])))))

(defn- copy-dir!
  "Recursively copy `src` into `dst`, creating `dst` if needed.
  Used to seed staging subdirectories with the previous export so an
  incremental run does not lose files it chooses not to rewrite (see
  the call site).  Copies file attributes so a seeded file the run
  keeps retains its mtime -- the web server derives ETag/Last-Modified
  from it, and conditional GETs would otherwise re-download unchanged
  files.  Relies on `file-seq`'s pre-order traversal so each
  directory is created before its children are copied."
  [^java.io.File src ^java.io.File dst]
  (when (.exists src)
    (let [src-path (.toPath src)
          dst-path (.toPath dst)
          opts     (into-array java.nio.file.CopyOption
                               [java.nio.file.StandardCopyOption/REPLACE_EXISTING
                                java.nio.file.StandardCopyOption/COPY_ATTRIBUTES])]
      (doseq [^java.io.File f (file-seq src)
              :let            [target (.resolve dst-path (.relativize src-path (.toPath f)))]]
        (if (.isDirectory f)
          (.mkdirs (.toFile target))
          (java.nio.file.Files/copy (.toPath f) target opts))))))

(defn- clean-stale-old-dirs!
  "Remove any leftover `<target>.old-*` directories from a previous
  crashed swap so they don't accumulate."
  [^java.io.File target]
  (let [parent (.getParentFile target)
        prefix (str (.getName target) ".old-")]
    (when (.isDirectory parent)
      (doseq [^java.io.File f (.listFiles parent)
              :when           (and (.isDirectory f)
                         (str/starts-with? (.getName f) prefix))]
        (delete-dir! f)))))

(defn- atomic-swap-dir!
  "Replace `target-dir` with `staging-dir`.
  Moves the old target to a temp name, moves staging into place,
  then deletes the old directory.  On failure of the second move,
  restores the previous target so callers never see a missing dir."
  [staging-dir target-dir]
  (let [target      (io/file target-dir)
        staging     (io/file staging-dir)
        old         (io/file (str target-dir ".old-" (System/currentTimeMillis)))
        had-target? (.exists target)]
    (clean-stale-old-dirs! target)
    (when had-target?
      (move-dir! target old))
    (try
      (move-dir! staging target)
      (when had-target?
        (delete-dir! old))
      (catch Exception e
        (when (and had-target? (.exists old) (not (.exists target)))
          (try (move-dir! old target)
               (catch Exception _
                 (log/error "Could not restore previous target from"
                            (.getAbsolutePath old)))))
        (throw e)))))

;; ---------------------------------------------------------------------------
;; --closed-retention: resolve a date or duration to a cutoff java.util.Date.
;; Reports closed before that date are excluded from export.
;; ---------------------------------------------------------------------------

(defn- drop-old-closed
  "Remove reports closed before `cutoff-date`."
  [reports ^java.util.Date cutoff-date]
  (remove (fn [r]
            (when-let [closed-email (:report/closed r)]
              (when-let [^java.util.Date closed-at (:email/date-sent closed-email)]
                (.before closed-at cutoff-date))))
          reports))

(defn- resolve-closed-retention [v]
  (when v
    (try
      (or (parse-cutoff-date v)
          (do
            (log/error "Invalid --closed-retention:" v
                       "(expected ISO date or duration like 90d, 6m, 1y)")
            (System/exit 1)))
      (catch Exception e
        (log/error "Invalid --closed-retention:" v "--" (.getMessage e))
        (System/exit 1)))))

(defn- resolve-topics-filter
  "Parse the CLI topics filter into a set of lower-cased topic strings.
  Accepts a comma-separated string.  Returns nil when no filter is active."
  [v]
  (when v
    (let [topics (->> (str/split v #",")
                      (map str/trim)
                      (remove str/blank?))]
      (when (seq topics)
        (set (map str/lower-case topics))))))

(defn- filter-by-topics
  "Keep only reports one of whose space-separated :report/topic-value
  tokens matches one of the given topics (case-insensitive).  Returns
  all reports when `topics` is nil."
  [reports topics]
  (if topics
    (filter (fn [r]
              (when-let [t (:report/topic-value r)]
                (some topics (str/split (str/lower-case t) #"\s+"))))
            reports)
    reports))

;; ---------------------------------------------------------------------------
;; DB queries
;; ---------------------------------------------------------------------------

(defn all-reports-by-date [db]
  (let [epoch (java.util.Date. 0)]
    (sort-by #(or (get-in % [:report/email :email/date-sent]) epoch)
             #(compare %2 %1)
             (all-reports db))))

;; ---------------------------------------------------------------------------
;; Formatting helpers
;; ---------------------------------------------------------------------------

(defn- attachment-basename
  "Return the basename of an attachment filename (handles absolute paths).
  Hostile or degenerate filenames (nil, \"\", \".\", \"..\") fall back to
  a stable name derived from a hash of the raw filename, so a crafted
  attachment can neither crash the export nor escape its directory."
  [att]
  (let [filename (:attachment/filename att)
        base     (some-> filename io/file .getName)]
    (if (or (nil? base) (#{"" "." ".."} base))
      (str "attachment-" (mid-hash (str filename)) ".txt")
      base)))

(defn- close-flag [report]
  (if (:report/closed report)
    (case (:report/close-reason report)
      :canceled   "C"
      :expired    "E"
      :superseded "S"
      "R")
    "-"))

(defn- flags-str [report]
  (str (if (:report/acked report) "A" "-")
       (if (:report/owned report) "O" "-")
       (close-flag report)))

(defn- votes-str
  "Format vote counts as \"score/total\" from a seq of vote maps, or nil."
  [votes]
  (when (seq votes)
    (let [{:keys [up down null]} (vote-counts votes)
          total                  (+ up down null)]
      (when (pos? total)
        (str (- up down) "/" total)))))

(defn- build-maintainers
  "Gather per-source currently-active maintainer sets from DB tenures.
   Returns source-name -> #{maintainer-emails}."
  [db source-map]
  (into {}
        (map (fn [[source-name _]]
               [source-name
                (->> (get-tenures db source-name)
                     (remove :to)
                     (keep :email)
                     (map str/lower-case)
                     set)]))
        source-map))

(defn- build-author-names
  "Build a {lowercased-addr -> latest-known author-name} map by scanning
  all emails.  Used to display role-bearing addresses (owner, etc.) by
  display name in exports rather than as bare email local-parts."
  [db]
  (let [pairs  (dq '[:find ?addr ?name ?date
                     :where
                     [?e :email/author-address ?addr]
                     [?e :email/author-name ?name]
                     [?e :email/date-sent ?date]]
                   db)
        latest (reduce (fn [m [addr name date]]
                         (let [k    (str/lower-case addr)
                               prev (get m k)]
                           (if (or (nil? prev)
                                   (.after ^java.util.Date date ^java.util.Date (second prev)))
                             (assoc m k [name date])
                             m)))
                       {} pairs)]
    (update-vals latest first)))

;; ---------------------------------------------------------------------------
;; Report -> map
;; ---------------------------------------------------------------------------

;; Defined with the export context below; declared here because the
;; report->map helpers above the context block already use it.
(declare get-header-cached)

(defn- archived-at [email]
  (get-header-cached (:email/headers-edn email) "Archived-At"))

(defn- raw-message-id
  "Original-case Message-Id from stored headers; nil for synthetic emails.
  Exports use this because public-inbox compares mids case-sensitively."
  [email]
  (extract-bracketed-id-raw
   (get-header-cached (:email/headers-edn email) "Message-Id")))

(defn- export-mid
  "Message-Id to expose in exports: original case when available, else
  the normalized DB key."
  [report email]
  (or (raw-message-id email) (:report/message-id report)))

(defn- sender-role
  "Determine role of sender for a given source context."
  [from source-name _source-map maintainers-map]
  (when (and (seq from) source-name)
    (let [from-lc (str/lower-case from)]
      (when (contains? (get maintainers-map source-name #{}) from-lc)
        "maintainer"))))

(def ^:private address-fields
  "Report address attrs to extract into the output map."
  [[:report/acked-address     :acked]
   [:report/owned-address     :owned]
   [:report/closed-address    :closed]
   [:report/urgent-address    :urgent]
   [:report/important-address :important]])

(def ^:private proxy-address-pairs
  "Triplets [ref-attr address-key proxy-key] for extracting the maintainer
  author-address.  The proxy key is only emitted when it differs from the
  corresponding address key (i.e. a `-by` directive pointed the attribute
  at someone other than the pose email's sender)."
  [[:report/acked     :acked     :acked-proxy]
   [:report/owned     :owned     :owned-proxy]
   [:report/closed    :closed    :closed-proxy]
   [:report/urgent    :urgent    :urgent-proxy]
   [:report/important :important :important-proxy]])

(defn- assoc-state-name
  "Attach `name-k` when the display name of the address under `addr-k`
  is known (looked up via the global author-names map built once per
  export)."
  [m addr-k name-k author-names]
  (if-let [addr (get m addr-k)]
    (if-let [nm (get author-names (str/lower-case addr))]
      (assoc m name-k nm)
      m)
    m))

(defn- assoc-state-names
  "Attach :owned-name and :acked-name when known."
  [m author-names]
  (-> m
      (assoc-state-name :owned :owned-name author-names)
      (assoc-state-name :acked :acked-name author-names)))

(defn- assoc-from-addresses
  "Extract addresses from report: direct string attrs and author-address of ref attrs.
  Omits -proxy keys when their value equals the corresponding address key."
  [m report]
  (as-> m m
    (reduce (fn [m [rk mk]]
              (if-let [v (get report rk)]
                (assoc m mk v)
                m))
            m address-fields)
    (reduce (fn [m [rk mk proxy-mk]]
              (if-let [v (get report rk)]
                (let [addr (:email/author-address v)]
                  (if (= addr (get m mk))
                    m
                    (assoc m proxy-mk addr)))
                m))
            m proxy-address-pairs)))

(defn- archive-url
  "Compute the archive URL for a report, or nil."
  [report email source-map]
  (let [source-name (:email/source email)
        src-type    (get-in source-map [source-name :source-type])]
    (when-not (#{:alias :mailbox} src-type)
      (let [;; Archived-At is sender-controlled and published as <a href>
            ;; and RSS <link>: http(s) only (no javascript:/data:).
            raw (when-let [aa (archived-at email)]
                  (when (re-matches #"(?i)https?://\S+" aa) aa))
            mid (some-> (export-mid report email) (str/replace #"^<|>$" ""))
            fmt (get-in source-map [source-name :archive-format-string])]
        (if (and fmt mid) (str/replace fmt "%s" mid) raw)))))

(defn- report-vote-fields
  "Build vote-related fields from vote data."
  [report-votes]
  (when (seq report-votes)
    (let [votes  (votes-str report-votes)
          counts (vote-counts report-votes)]
      (cond-> {}
        votes                   (assoc :votes votes)
        (pos? (:up counts 0))   (assoc :votes-up (:up counts))
        (pos? (:down counts 0)) (assoc :votes-down (:down counts))
        (pos? (:null counts 0)) (assoc :votes-null (:null counts))))))

(defn- report-series-fields [series]
  (when series
    (let [patches (:series/patches series)]
      {:id       (:series/id series)
       :received (count patches)
       :expected (:series/expected series)
       :complete (= (count patches) (:series/expected series))
       :closed   (some? (:series/closed series))})))

(defn- report-patch-fields [report]
  (when (seq (:report/patches report))
    (let [h (mid-hash (:report/message-id report))]
      (mapv (fn [p]
              (cond-> {:file   (str h "/" (:patch/filename p))
                       :source (name (:patch/source p))}
                (:patch/author p)  (assoc :author  (:patch/author p))
                (:patch/subject p) (assoc :subject (:patch/subject p))
                (:patch/date p)    (assoc :date    (:patch/date p))))
            (:report/patches report)))))

(def ^:private inline-ics-basename "inline.ics")

(defn- event-ics-files
  "The ICS files to publish for one announcement, as
  [{:basename ... :content ...}].  .ics attachments are published as-is
  (only CRLF-normalized); when an announcement carries no .ics attachment but has
  inline VEVENT content, a single synthetic inline.ics is built from the
  body (VEVENTs deduped by UID, referenced VTIMEZONEs carried along).
  Shared by dump-events! (writes) and report-attachment-files (lists) so
  the files on disk and the :events JSON entries never diverge."
  [report att-email]
  (let [ics-atts (filter #(and (ics-file? (:attachment/filename %))
                               (:attachment/data %))
                         (:email/attachments att-email))]
    (if (seq ics-atts)
      ;; Attachments are published essentially as-is; only line endings are
      ;; normalized to CRLF (RFC 5545 requires it) so per-report files and
      ;; the combined calendars agree on EOLs.
      (mapv (fn [att] {:basename (attachment-basename att)
                       :content  (normalize-ics-eol (:attachment/data att))})
            ics-atts)
      (let [body    (email-body-text att-email)
            vevents (dedupe-vevents (extract-vevents body))]
        (when (seq vevents)
          (let [cal-name (or (get-in report [:report/email :email/subject]) "event")]
            [{:basename inline-ics-basename
              :content  (build-vcalendar cal-name vevents
                                         (dedupe-vtimezones (extract-vtimezones body)))}]))))))

(defn- report-attachment-files
  "Build :events and :texts fields from lazy-fetched attachment data."
  [report att-email]
  (let [h     (mid-hash (:report/message-id report))
        texts (when (:report/has-text-attachments report)
                (mapv (fn [att] {:file (str h "/" (attachment-basename att))})
                      (filter #(and (text-attachment? %) (:attachment/data %))
                              (:email/attachments @att-email))))]
    (cond-> {}
      (and (= :announcement (:report/type report))
           (:report/has-ics report))
      (assoc :events
             (mapv (fn [f] {:file (str h "/" (:basename f))})
                   (event-ics-files report @att-email)))
      (seq texts)
      (assoc :texts texts))))

;; ---------------------------------------------------------------------------
;; Export context -- bound for the duration of one export run.
;; Used by map-reports, dump-events!, dump-text!, and dump-events-ics!
;; to access the DB and votes without threading parameters through
;; every dump-* function.
;; ---------------------------------------------------------------------------

(def ^:dynamic ^:private *export-ctx*
  "Export context: {:db <datalevin-db> :votes {eid -> [vote-maps]}
  :config <config> :author-names {lc-addr -> display-name}
  :report-cache (atom {eid -> exported-map})
  :headers-cache (atom {headers-edn-string -> parsed-pairs})}"
  {:db nil :votes {} :config nil :author-names {}
   :report-cache (atom {}) :headers-cache (atom {})})

(defn- build-export-ctx [db votes config]
  {:db           db
   :votes        votes
   :config       config
   :author-names (merge (build-author-names db) (load-mailmap))
   ;; report->map is pure given the (stable) context, yet every output
   ;; file re-maps its slice -- a closed patch is mapped for all.json,
   ;; all.org, patches.json, patches.org, all-closed.*, patches-closed.*
   ;; etc.  Cache by eid so each report is mapped exactly once per run.
   :report-cache  (atom {})
   ;; headers-edn is a ~5KB EDN string per email; parsing it dominates
   ;; the mapping pass (archived-at + raw-message-id, plus relation
   ;; references to the same email).  Cache the parse by string content.
   :headers-cache (atom {})})

(defn- ctx-db [] (:db *export-ctx*))
(defn- ctx-votes [] (:votes *export-ctx*))
(defn- ctx-config [] (:config *export-ctx*))
(defn- ctx-author-names [] (:author-names *export-ctx*))
(defn- ctx-report-cache [] (:report-cache *export-ctx*))
(defn- ctx-headers-cache [] (:headers-cache *export-ctx*))

(defn- get-header-cached
  "Like `common/get-header`, but parses each headers-edn string at most
  once per export run.  Header parsing (edn/read-string of a ~5KB
  string) otherwise dominates the report->map pass at scale."
  [headers-edn header-name]
  (when headers-edn
    (let [cache  (ctx-headers-cache)
          parsed (or (get @cache headers-edn)
                     (let [p (parse-headers headers-edn)]
                       (swap! cache assoc headers-edn p)
                       p))
          lname  (str/lower-case header-name)]
      (some (fn [[k v]] (when (= (str/lower-case k) lname) v)) parsed))))

(def ^:private default-awaiting-delay-days 14)

(defn- awaiting-reply?
  "True when a report is open, last activity was by a maintainer,
  the OP is not a maintainer, and the configured delay has elapsed.
  When the OP is themselves a maintainer, \"awaiting reply\" makes no
  semantic sense -- we are not waiting for a maintainer to answer their
  own thread."
  [report source-name source-map maintainers-map]
  (when (and (not (:report/closed report))
             (:report/last-activity report)
             (:report/last-activity-address report))
    (let [last-addr (str/lower-case (:report/last-activity-address report))
          op-addr   (some-> (get-in report [:report/email :email/author-address])
                            str/lower-case)
          last-role (sender-role last-addr source-name source-map maintainers-map)
          op-role   (when op-addr
                      (sender-role op-addr source-name source-map maintainers-map))]
      (when (and last-role (not op-role))
        (let [config                   (ctx-config)
              src-cfg                  (get source-map source-name)
              delay-str                (or (:awaiting-delay src-cfg)
                             (:awaiting-delay config))
              ;; Guard both failure modes of parse-delay: nil on a
              ;; valueless string, throw on unknown units.
              delay-days               (or (when delay-str
                                             (try (parse-delay delay-str)
                                                  (catch Exception _ nil)))
                                           default-awaiting-delay-days)
              ^java.util.Date last-act (:report/last-activity report)
              elapsed-ms               (- (System/currentTimeMillis) (.getTime last-act))
              elapsed-days             (/ elapsed-ms (* 24 60 60 1000))]
          (>= elapsed-days delay-days))))))

(def ^:private export-rel-key
  "Public names for relation kinds.  The supersedes pair is stored from
  the closed report's side (closure-relation-rows: :rel/from = the
  report being closed, :rel/kind :supersedes), so read literally the
  closed report would export \"supersedes\" and its replacement
  \"superseded-by\" -- backwards for any reader.  Flip the pair at the
  export boundary; :resolves and :duplicates already read correctly."
  {:supersedes    :superseded-by
   :superseded-by :supersedes})

(defn- group-relations
  "Build the per-kind relation summary for a pulled report.
  Outgoing relations (:rel/_from) cover all kinds; incoming :related-to
  (:rel/_to) is added because the symmetric kind canonicalises to the
  smaller-eid side, so the other report only sees it via incoming."
  [report source-map]
  (let [self-eid        (:db/id report)
        ;; Go through archive-url so linked reports get the same
        ;; treatment as the report itself: https-only validation of the
        ;; sender-controlled Archived-At header and :archive-format-string
        ;; substitution, keyed on the linked report's own source.
        archive         (fn [other-r]
                          (archive-url other-r (:report/email other-r) source-map))
        format-rel      (fn [from-side? rel]
                          (let [other (if from-side? (:rel/to rel) (:rel/from rel))
                                a     (archive other)
                                subj  (get-in other [:report/email :email/subject])
                                mid   (export-mid other (:report/email other))]
                       (cond-> {:message-id mid}
                         (:report/type other) (assoc :type (name (:report/type other)))
                         subj                 (assoc :subject subj)
                         a                    (assoc :archived-at a)
                         (:rel/setter rel)    (assoc :setter (:rel/setter rel))
                         (:rel/posed-at rel)  (assoc :posed-at (format-date-iso (:rel/posed-at rel)))
                         (:rel/value rel)     (assoc :value (:rel/value rel)))))
        active-not-self (fn [other-key]
                          (filter #(and (:rel/active? %)
                                        (not= self-eid (:db/id (other-key %))))))
        out             (sequence (comp (active-not-self :rel/to)
                                   (map (juxt :rel/kind #(format-rel true %))))
                             (:rel/_from report))
        in-related      (sequence (comp (active-not-self :rel/from)
                                   (filter #(= :related-to (:rel/kind %)))
                                   (map (juxt :rel/kind #(format-rel false %))))
                             (:rel/_to report))]
    (reduce-kv (fn [m k entries]
                 (let [k' (keyword (name k))]
                   (assoc m (export-rel-key k' k') (mapv second entries))))
               {}
               (group-by first (concat out in-related)))))

(defn report->map [report source-map maintainers-map report-votes db]
  (let [email       (:report/email report)
        source-name (:email/source email)
        att-data    (delay (when db
                             (fetch-attachment-data db (:report/message-id report))))
        att-email   (delay (:report/email @att-data))
        from        (or (:email/author-address email) "")
        from-name   (or (get (ctx-author-names) (str/lower-case from))
                        (:email/author-name email))
        arch        (archive-url report email source-map)
        relations   (group-relations report source-map)
        role        (sender-role from source-name source-map maintainers-map)
        awaiting?   (awaiting-reply? report source-name source-map maintainers-map)]
    (-> {:type     (name (:report/type report))
         :subject  (or (:email/subject email) "")
         :from     from
         :date     (format-date (:email/date-sent email))
         ;; Keep the Date object itself for internal formatters (RSS, Org).
         ;; Cheshire serializes java.util.Date to ISO-8601 in JSON output.
         :date-raw (:email/date-sent email)
         :flags    (flags-str report)
         :status   (report-status report)
         :priority (report-priority report)
         :replies  (report-descendant-count report)}
        (assoc-from-addresses report)
        (assoc-state-names (ctx-author-names))
        (cond->
            from-name                       (assoc :from-name from-name)
            role                            (assoc :role role)
            (:report/message-id report)     (assoc :message-id (export-mid report email))
            (:report/version report)        (assoc :version (:report/version report))
            (:report/topic-value report)    (assoc :topic (:report/topic-value report))
            (:report/patch-seq report)      (assoc :patch-seq (:report/patch-seq report))
            (:report/patch-source report)   (assoc :patch-source (mapv name (sort (:report/patch-source report))))
            ;; Sorted for reproducible exports (cardinality-many = set).
            (seq (:report/trailers report)) (assoc :trailers (vec (sort (:report/trailers report))))
            arch                            (assoc :archived-at arch)
            (:report/deadline-value report) (assoc :deadline (format-date-iso (:report/deadline-value report)))
            (:report/last-activity report)  (assoc :last-activity (format-date-iso (:report/last-activity report)))
            awaiting?                       (assoc :awaiting true)
            (:report/expiry-value report)   (assoc :expiry (format-date-iso (:report/expiry-value report)))
            (:report/close-reason report)   (assoc :close-reason (name (:report/close-reason report)))
            (and (= :expired (:report/close-reason report))
                 (:email/date-sent (:report/closed report)))
            (assoc :expired-date (format-date-iso (:email/date-sent (:report/closed report)))))
        (merge (report-vote-fields report-votes))
        (cond->
            (:report/series report) (assoc :series (report-series-fields (:report/series report))))
        ;; Qualified relations exported as one field per kind.
        (merge relations)
        (cond->
            (seq (:report/patches report)) (assoc :patches (report-patch-fields report)))
        (merge (report-attachment-files report att-email))
        ;; Ensure closed is truthy when close-reason is set (auto-superseded
        ;; reports may lack :report/closed-address, leaving :closed unset).
        (as-> m (if (and (:close-reason m) (not (:closed m)))
                  (assoc m :closed "auto")
                  m)))))

(defn- map-reports
  "Map reports through report->map, looking up votes from export context.
  Memoized by report eid for the duration of the export run, so the
  many output files that cover overlapping report slices each pay the
  mapping cost only once."
  [reports source-map maintainers-map]
  (let [av    (ctx-votes)
        db    (ctx-db)
        cache (ctx-report-cache)]
    (mapv (fn [r]
            (let [eid (:db/id r)]
              (or (get @cache eid)
                  (let [m (report->map r source-map maintainers-map (get av eid) db)]
                    (swap! cache assoc eid m)
                    m))))
          reports)))

;; ---------------------------------------------------------------------------
;; Source metadata for JSON envelope
;; ---------------------------------------------------------------------------

(defn- source-metadata
  "Build metadata map for a single source."
  [source-name source-map]
  (let [cfg (get source-map source-name)]
    (cond-> {:source-type (or (:source-type cfg) :mailbox)}
      (:list cfg)         (assoc :list          (:list cfg))
      (:alias cfg)        (assoc :alias         (:alias cfg))
      (:to cfg)           (assoc :to            (:to cfg))
      (:list-archive cfg) (assoc :list-archive  (:list-archive cfg))
      (:archive-format-string cfg) (assoc :archive-format-string (:archive-format-string cfg))
      (:base-url cfg)     (assoc :base-url      (:base-url cfg))
      (:website cfg)        (assoc :website        (:website cfg))
      (:contribute-url cfg) (assoc :contribute-url (:contribute-url cfg)))))

;; ---------------------------------------------------------------------------
;; XML helpers
;; ---------------------------------------------------------------------------

(defn- xml-escape [s]
  (when s
    (-> s
        (str/replace "&" "&amp;")
        (str/replace "<" "&lt;")
        (str/replace ">" "&gt;")
        (str/replace "\"" "&quot;")
        (str/replace "'" "&apos;"))))

;; ---------------------------------------------------------------------------
;; Filtering
;; ---------------------------------------------------------------------------

(defn- filter-reports
  "Filter reports by optional criteria: source name, type, min priority/status."
  [reports {:keys [source type min-priority min-status]}]
  (cond->> reports
    source       (filter #(= source (get-in % [:report/email :email/source])))
    type         (filter #(= type (:report/type %)))
    min-priority (filter #(>= (report-priority %) min-priority))
    min-status   (filter #(>= (report-status %) min-status))))

(defn- open-reports
  "Keep only reports that are not closed."
  [reports]
  (remove :report/closed reports))

(defn- updated-since?
  "True when a report changed after `since`, so its per-mid files
  (patches/, text/, events/) must be (re)written.  A nil `since` means
  no incremental cutoff (write everything).  A report without
  :report/updated-at is treated as changed -- we never skip a write we
  are unsure about."
  [report ^java.util.Date since]
  (if-let [^java.util.Date updated (:report/updated-at report)]
    (or (nil? since) (.after updated since))
    true))

;; ---------------------------------------------------------------------------
;; Per-type helpers
;; ---------------------------------------------------------------------------

(def report-types
  "Ordered keywords of every report type, driven by `common/report-type-spec`."
  (mapv :type report-type-spec))

(def rss-limit 50)

;; ---------------------------------------------------------------------------
;; Per-source export functions
;; ---------------------------------------------------------------------------

(defn- json-unchanged?
  "True when `file` already holds the JSON we are about to write.
  Incremental staging is seeded from the previous export with mtimes
  preserved, so leaving an identical file untouched keeps its mtime
  and thus the ETag/Last-Modified the web server derives from it.
  When `envelope` carries the per-run :generated stamp, compare the
  parsed data without it: skipped runs already leave the stamp stale,
  so it effectively tracks the last time the data changed."
  [file json-str envelope]
  (let [f (io/file file)]
    (and (.exists f)
         (let [prev (slurp f)]
           (if (contains? envelope :generated)
             (try (= (dissoc (json/parse-string prev true) :generated)
                     (dissoc (json/parse-string json-str true) :generated))
                  (catch Exception _ false))
             (= prev json-str))))))

(defn dump-json!
  "Dump reports as JSON for a single source.
  Leaves a file whose content did not change untouched (see
  `json-unchanged?`)."
  ([reports out-dir source-name source-map maintainers-map]
   (dump-json! reports out-dir source-name source-map maintainers-map "all.json" nil))
  ([reports out-dir source-name source-map maintainers-map basename]
   (dump-json! reports out-dir source-name source-map maintainers-map basename nil))
  ([reports out-dir source-name source-map maintainers-map basename extra-meta]
   (let [data     (map-reports reports source-map maintainers-map)
         meta     (source-metadata source-name source-map)
         envelope (cond-> {:bone-format bone-format
                           :source      source-name
                           :reports     data}
                    (seq meta)       (merge meta)
                    (seq extra-meta) (merge extra-meta))
         filename (str out-dir "/" basename)
         json-str (json/generate-string envelope {:pretty true})]
     (if (json-unchanged? filename json-str envelope)
       (log/info "Unchanged" filename)
       (do (spit filename json-str)
           (when (seq data)
             (log/info "Wrote" (count data) "reports to" filename)))))))

(def ^:private rfc822-formatter
  ;; DateTimeFormatter is immutable and thread-safe.  Pattern matches
  ;; SimpleDateFormat's "EEE, dd MMM yyyy HH:mm:ss Z" output (e.g.
  ;; "Mon, 12 May 2026 14:30:45 +0000") -- do not substitute with
  ;; RFC_1123_DATE_TIME, which emits "GMT" and a non-padded day.
  (-> (java.time.format.DateTimeFormatter/ofPattern "EEE, dd MMM yyyy HH:mm:ss Z")
      (.withLocale java.util.Locale/ENGLISH)
      (.withZone java.time.ZoneOffset/UTC)))

(defn- rfc822-date
  "Format a java.util.Date as an RFC 822 date string (for RSS)."
  [^java.util.Date d]
  (when d
    (.format rfc822-formatter (.toInstant d))))

(defn- rss-author [m]
  (let [email (:from m)
        name  (:from-name m)]
    (xml-escape (if (and name (not= name email))
                  (str email " (" name ")")
                  email))))

(defn- rss-guid [m]
  (let [arch (:archived-at m)
        mid  (:message-id m)]
    (cond
      (seq arch) {:value (xml-escape arch) :permalink true}
      mid        {:value (xml-escape (str "urn:message-id:" mid)) :permalink false}
      :else      nil)))

(defn- report->rss-item [m]
  (let [title   (xml-escape (:subject m))
        link    (or (:archived-at m) "")
        date    (rfc822-date (:date-raw m))
        author  (rss-author m)
        guid    (rss-guid m)
        flags   (:flags m)
        replies (:replies m)
        desc    (xml-escape
                 (str "[" (:type m) "] flags:" flags " replies:" replies
                      (when-let [r (:role m)]     (str " role:" r))
                      (when-let [v (:version m)]  (str " version:" v))
                      (when-let [t (:topic m)]    (str " topic:" t))
                      (when-let [d (:deadline m)] (str " deadline:" d))
                      (when-let [d (:expiry m)]   (str " expiry:" d))
                      (when-let [s (:series-summary m)] (str " series:" s))))]
    (str "    <item>\n"
         "      <title>" title "</title>\n"
         (when (seq link)
           (str "      <link>" (xml-escape link) "</link>\n"))
         (when guid
           (if (:permalink guid)
             (str "      <guid>" (:value guid) "</guid>\n")
             (str "      <guid isPermaLink=\"false\">" (:value guid) "</guid>\n")))
         (when date
           (str "      <pubDate>" date "</pubDate>\n"))
         (when-let [role (:role m)]
           (str "      <category>" (xml-escape role) "</category>\n"))
         "      <author>" author "</author>\n"
         "      <description>" desc "</description>\n"
         "    </item>")))

(defn- series-status-summary
  "Build a short status summary for a folded series, e.g. \"3/5 acked, 1 open\".
  Excludes cover letters (patch-seq starting with \"0/\") from the tally."
  [members]
  (let [patches (remove #(str/starts-with? (or (:patch-seq %) "") "0/") members)
        counts  (reduce (fn [acc m]
                          (let [flags (or (:flags m) "---")]
                           (cond
                             (not= \- (nth flags 2 \-)) (update acc :closed (fnil inc 0))
                             (not= \- (nth flags 0 \-)) (update acc :acked (fnil inc 0))
                             :else                      (update acc :open (fnil inc 0)))))
                        {} patches)
        parts   (cond-> []
                  (:acked counts)  (conj (str (:acked counts) " acked"))
                  (:closed counts) (conj (str (:closed counts) " closed"))
                  (:open counts)   (conj (str (:open counts) " open")))]
    (str/join ", " parts)))

(defn- fold-series
  "Fold report maps by series: replace each series group with a single
  representative (cover letter preferred, else first patch by patch-seq).
  Non-series reports pass through unchanged.  Preserves original order
  (the representative appears at the position of the first group member)."
  [report-maps]
  (let [groups (group-by #(get-in % [:series :id]) report-maps)
        seen   (volatile! #{})]
    (reduce (fn [acc m]
              (let [sid (get-in m [:series :id])]
                (if-not sid
                  (conj acc m)
                  (if (contains? @seen sid)
                    acc ;; already emitted representative
                    (let [_       (vswap! seen conj sid)
                          members (get groups sid)
                          sorted  (sort-by #(or (some-> (re-find #"^\d+" (or (:patch-seq %) ""))
                                                        parse-long)
                                                0)
                                           members)
                          cover   (first (filter #(str/starts-with? (or (:patch-seq %) "") "0/") sorted))
                          rep     (or cover (first sorted))
                          summary (series-status-summary members)]
                      (conj acc (assoc rep :series-summary summary)))))))
            [] report-maps)))

(defn- delete-stale-file!
  "Delete a previously-seeded export file.  Incremental staging dirs are
  seeded from the previous export; a file whose data slice became empty
  is no longer rewritten and must be deleted, or the atomic swap would
  keep publishing its old content forever."
  [dir basename]
  (let [f (io/file dir basename)]
    (when (.exists f)
      (io/delete-file f true)
      (log/info "Deleted stale" basename))))

(defn- delete-stale-typed!
  "delete-stale-file! for `stem` across all per-type export formats."
  [dir stem]
  (doseq [ext [".json" ".xml" ".org"]]
    (delete-stale-file! dir (str stem ext))))

(defn dump-rss!
  "Dump reports as RSS 2.0 for a single source.
  Only includes open reports, capped at rss-limit (50).
  Patch series are folded: one item per series."
  ([reports out-dir source-name source-map maintainers-map]
   (dump-rss! reports out-dir source-name source-map maintainers-map "all.xml" "reports"))
  ([reports out-dir source-name source-map maintainers-map basename feed-label]
   (let [;; 20x headroom: series folding can collapse many reports into
         ;; one item, a plain (take rss-limit) could starve the final cap.
         open     (->> reports open-reports (take (* rss-limit 20)))
         data     (map-reports open source-map maintainers-map)
         data     (->> data fold-series (take rss-limit))
         items    (str/join "\n" (map report->rss-item data))
         list-url (get-in source-map [source-name :list-archive] "")
         filename (str out-dir "/" basename)]
     (if (seq data)
       (do (spit filename
                 (str "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
                      "<rss version=\"2.0\">\n"
                      "  <channel>\n"
                      "    <title>BONE " (xml-escape source-name) " " (xml-escape feed-label) "</title>\n"
                      "    <link>" (xml-escape list-url) "</link>\n"
                      "    <description>Reports from the Bug And Report Keeper</description>\n"
                      items "\n"
                      "  </channel>\n"
                      "</rss>\n"))
           (log/info "Wrote" (count data) "reports to" filename))
       ;; No open reports left: drop the seeded feed instead of letting
       ;; it keep announcing reports closed since the last export.
       (delete-stale-file! out-dir basename)))))

(defn- format-org-inactive-ts
  "Format a java.util.Date as an Org inactive timestamp like
  [2026-03-12 Thu 20:05]."
  [^java.util.Date d]
  (when d
    (let [out-fmt (doto (java.text.SimpleDateFormat. "yyyy-MM-dd EEE HH:mm"
                                                     java.util.Locale/ENGLISH)
                    (.setTimeZone (java.util.TimeZone/getTimeZone "UTC")))]
      (str "[" (.format out-fmt d) "]"))))

(defn- strip-angle-brackets [s]
  (when s (str/replace s #"^<|>$" "")))

(defn- org-safe
  "Strip characters that would break Org structure: newlines (which
  would split a headline or property) and the :END: token (which
  would close a PROPERTIES drawer prematurely)."
  [s]
  (when s
    (-> (str s)
        (str/replace #"[\r\n]+" " ")
        (str/replace #":END:" ":END :"))))

(def ^:private org-property-rows
  "Ordered :PROPERTIES: entries used by `report->org-entry`.  Each row:
  `[<key in report-map> <:LABEL:> <xf> <default>]`.
  An entry is emitted when `(get m key default)` is non-nil; `xf`, when
  non-nil, transforms the value before stringification.  Vector order
  dictates property-drawer order."
  [[:from         "FROM"         org-safe                                  ""]
   [:date-raw     "DATE"         format-org-inactive-ts                    nil]
   [:message-id   "MESSAGE-ID"   #(some-> % strip-angle-brackets org-safe) nil]
   [:archived-at  "ARCHIVED-AT"  org-safe                                  nil]
   [:flags        "FLAGS"        nil                                       "---"]
   [:status       "STATUS"       nil                                       0]
   [:replies      "REPLIES"      nil                                       0]
   [:version      "VERSION"      org-safe                                  nil]
   [:topic        "TOPIC"        org-safe                                  nil]
   [:votes        "VOTES"        nil                                       nil]
   [:votes-up     "VOTES-UP"     nil                                       nil]
   [:votes-down   "VOTES-DOWN"   nil                                       nil]
   [:votes-null   "VOTES-NULL"   nil                                       nil]
   [:acked        "ACKED"        org-safe                                  nil]
   [:owned        "OWNED"        org-safe                                  nil]
   [:closed       "CLOSED"       org-safe                                  nil]
   [:close-reason "CLOSE-REASON" nil                                       nil]
   [:urgent       "URGENT"       org-safe                                  nil]
   [:important    "IMPORTANT"    org-safe                                  nil]
   [:deadline     "DEADLINE"     nil                                       nil]
   [:expiry       "EXPIRY"       nil                                       nil]])

(defn- org-property-line
  "Render one :PROPERTIES: entry from a row of `org-property-rows`,
  or `nil` to skip it."
  [m [k label xf default]]
  (let [raw (get m k default)]
    (when (some? raw)
      (str ":" label ": " (if xf (xf raw) raw)))))

(defn- report->org-entry [m]
  (let [;; Flag position 2 is the close flag: C/E/S/R, or - when open.
        todo    (if (= (nth (:flags m "---") 2 \-) \-) "TODO" "DONE")
        prio    (case (:priority m 0)
                  3 "[#A] " 2 "[#B] " 1 "[#C] " "")
        subject (org-safe (:subject m ""))
        tags    (when-let [t (:type m)] (str ":" t ":"))
        props   (cond-> (keep #(org-property-line m %) org-property-rows)
                  (:series m)
                  (concat [(let [s (:series m)]
                             (str ":SERIES: " (:received s) "/" (:expected s)
                                  (when (:closed s) " closed")))]))]
    (str "* " todo " " prio subject (when tags (str "  " tags)) "\n"
         (when-let [d (:deadline m)]
           (str "DEADLINE: <" d ">\n"))
         ":PROPERTIES:\n"
         (str/join "\n" props) "\n"
         ":END:\n"
         (let [;; Union of all qualified-relation kinds, deduped by mid.
               all-related (->> [:resolves :resolved-by
                                 :supersedes :superseded-by
                                 :duplicates :duplicated-by
                                 :related-to]
                                (mapcat #(get m %))
                                (filter :message-id)
                                (group-by :message-id)
                                vals
                                (map first))]
           (when (seq all-related)
             (str "\nRelated:\n"
                  (str/join "\n"
                            (map (fn [r]
                                   (str "- [" (:type r) "] " (:message-id r)
                                        (when-let [a (:archived-at r)]
                                          (str " (" a ")"))))
                                 all-related))
                  "\n"))))))

(defn dump-org!
  "Dump reports as Org for a single source."
  ([reports out-dir source-name source-map maintainers-map]
   (dump-org! reports out-dir source-name source-map maintainers-map "all.org" "reports"))
  ([reports out-dir source-name source-map maintainers-map basename title-label]
   (let [data     (map-reports reports source-map maintainers-map)
         filename (str out-dir "/" basename)]
     (if (seq data)
       (let [entries (str/join "\n" (map report->org-entry data))]
         (spit filename
               (str "#+TITLE: BONE " source-name " " title-label "\n"
                    "#+DATE: " (java.time.LocalDate/now) "\n\n"
                    entries))
         (log/info "Wrote" (count data) "reports to" filename))
       (delete-stale-file! out-dir basename)))))

(defn dump-votes!
  "Export votes.json for a single source.
  Format: {\"<mid>\": {\"+1\": [{voter, message-id}], \"-1\": [...], \"0\": [...]}}"
  [reports out-dir]
  (let [av      (ctx-votes)
        entries (reduce
                  (fn [acc report]
                    (let [mid   (export-mid report (:report/email report))
                          votes (get av (:db/id report))]
                      (if (seq votes)
                        (let [grouped (group-by :value votes)
                              fmt     (fn [vs]
                                        (mapv (fn [{:keys [voter email-mid-raw email-mid]}]
                                                {:voter voter
                                                 :message-id (or email-mid-raw email-mid)})
                                              vs))]
                          (assoc acc mid {"+1" (fmt (:up grouped))
                                          "-1" (fmt (:down grouped))
                                          "0"  (fmt (:null grouped))}))
                        acc)))
                  {}
                  reports)]
    (if (seq entries)
      (let [filename (str out-dir "/votes.json")]
        (spit filename (json/generate-string entries {:pretty true}))
        (log/info "Wrote" (count entries) "report(s) with votes to" filename))
      (delete-stale-file! out-dir "votes.json"))))

(defn- patch-basename
  "Return the basename of a :report/patches entry (handles absolute paths)."
  [p]
  (.getName (io/file (:patch/filename p))))

(defn dump-patches!
  "Export patch files for a single source.
  Patch text is not in the default pull pattern, so it is fetched on
  demand here -- only for the reports actually written.  With `:since`,
  only reports updated after it are (re)written; the rest survive in
  staging via the seeded patches/ directory."
  [reports patches-dir & {:keys [since]}]
  (let [db    (ctx-db)
        reps  (cond->> (filter #(seq (:report/patches %)) reports)
                since (filter #(updated-since? % since)))
        total (reduce (fn [n report]
                        (let [h       (mid-hash (:report/message-id report))
                              dir     (io/file patches-dir h)
                              patches (:report/patches
                                       (dpull db '[{:report/patches [:patch/filename :patch/text]}]
                                              (:db/id report)))]
                          (.mkdirs dir)
                          (doseq [p patches]
                            (spit (io/file dir (patch-basename p)) (:patch/text p)))
                          (+ n (count patches))))
                      0
                      reps)]
    (when (pos? total)
      (log/info "Wrote" total "patch file(s)"))))

;; ---------------------------------------------------------------------------
;; Attachment batch fetch (shared by text and event exports)
;; ---------------------------------------------------------------------------

(defn- batch-fetch-attachments
  "Fetch attachment data for a seq of reports. Returns {message-id -> att-email}."
  [reports]
  (let [db (ctx-db)]
    (into {}
          (keep (fn [report]
                  (let [mid (:report/message-id report)]
                    (when-let [att (fetch-attachment-data db mid)]
                      [mid (:report/email att)]))))
          reports)))

(defn dump-text!
  "Export text attachments (text/plain, text/x-log) to text/<mid-hash>/.
  With `:since`, only reports updated after it are (re)written; the
  rest survive in staging via the seeded text/ directory."
  [reports text-dir & {:keys [since]}]
  (let [txt-reports (cond->> (filter :report/has-text-attachments reports)
                      since (filter #(updated-since? % since)))
        att-cache   (batch-fetch-attachments txt-reports)
        total       (reduce (fn [n report]
                              (let [txt-atts (filter #(and (text-attachment? %)
                                                           (:attachment/data %))
                                                     (:email/attachments (get att-cache (:report/message-id report))))]
                          (if (seq txt-atts)
                            (let [h   (mid-hash (:report/message-id report))
                                  dir (io/file text-dir h)]
                              (.mkdirs dir)
                              (doseq [att txt-atts]
                                (spit (io/file dir (attachment-basename att))
                                      (:attachment/data att)))
                              (+ n (count txt-atts)))
                            n)))
                      0
                      txt-reports)]
    (when (pos? total)
      (log/info "Wrote" total "text file(s)"))))

;; ---------------------------------------------------------------------------
;; Event (ICS) export
;; ---------------------------------------------------------------------------

(defn- ics-announcements
  "Announcements flagged (at ingest) as carrying ICS content."
  [reports]
  (filter #(and (= :announcement (:report/type %)) (:report/has-ics %)) reports))

(defn dump-events!
  "Export individual .ics files to events/<mid-hash>/ for announcements with ICS.
  With `:since`, only reports updated after it are (re)written; the
  rest survive in staging via the seeded events/ directory."
  [reports events-dir & {:keys [since]}]
  (let [ics-reports (cond->> (ics-announcements reports)
                      since (filter #(updated-since? % since)))
        att-cache   (batch-fetch-attachments ics-reports)
        total       (reduce (fn [n report]
                              (let [files (event-ics-files
                                           report (get att-cache (:report/message-id report)))]
                          (if (seq files)
                            (let [h   (mid-hash (:report/message-id report))
                                  dir (io/file events-dir h)]
                              (.mkdirs dir)
                              (doseq [{:keys [basename content]} files]
                                (spit (io/file dir basename) content))
                              (+ n (count files)))
                            n)))
                      0
                      ics-reports)]
    (when (pos? total)
      (log/info "Wrote" total "ICS event file(s)"))))

(defn dump-events-filtered!
  "Export events.json/org (open) and events-closed.json/org (closed)
  for announcements that have ICS content."
  [reports reports-dir source-name source-map maintainers-map fmts]
  (let [events        (ics-announcements reports)
        open-events   (vec (open-reports events))
        closed-events (vec (filter :report/closed events))]
    (if (seq open-events)
      (do
        (when (fmts "json")
          (dump-json! open-events reports-dir source-name source-map maintainers-map
                      "events.json"))
        (when (fmts "org")
          (dump-org! open-events reports-dir source-name source-map maintainers-map
                     "events.org" "events"))
        (when (fmts "rss")
          (dump-rss! open-events reports-dir source-name source-map maintainers-map
                     "events.xml" "events")))
      (do
        (delete-stale-file! reports-dir "events.json")
        (delete-stale-file! reports-dir "events.org")
        (delete-stale-file! reports-dir "events.xml")))
    (if (seq closed-events)
      (do
        (when (fmts "json")
          (dump-json! closed-events reports-dir source-name source-map maintainers-map
                      "events-closed.json"))
        (when (fmts "org")
          (dump-org! closed-events reports-dir source-name source-map maintainers-map
                     "events-closed.org" "events (closed)"))
        (when (fmts "rss")
          (dump-rss! closed-events reports-dir source-name source-map maintainers-map
                     "events-closed.xml" "events (closed)")))
      (do
        (delete-stale-file! reports-dir "events-closed.json")
        (delete-stale-file! reports-dir "events-closed.org")
        (delete-stale-file! reports-dir "events-closed.xml")))))

(defn- report-vcal-blocks
  "Extract {:vevents [...] :vtimezones [...]} (CRLF-normalized) from a
  report's ICS sources: every .ics attachment plus the inline body.  The
  inline body is read through `email-body-text` so detection and
  extraction agree on what counts as body content."
  [att-email]
  (let [ics-texts (conj (->> (:email/attachments att-email)
                             (filter #(and (ics-file? (:attachment/filename %))
                                           (:attachment/data %)))
                             (mapv :attachment/data))
                        (email-body-text att-email))]
    {:vevents    (mapcat extract-vevents ics-texts)
     :vtimezones (mapcat extract-vtimezones ics-texts)}))

(defn dump-events-ics!
  "Export combined ICS calendars for announcements with VEVENT content:
   announcements.ics (all), -open.ics, -closed.ics.  Attachments are
   fetched once; VEVENTs are deduplicated by UID (highest SEQUENCE wins)
   and the VTIMEZONE definitions they reference are carried along."
  [reports events-dir source-name]
  (let [events    (ics-announcements reports)
        att-cache (batch-fetch-attachments events)
        tagged    (mapv (fn [r]
                          (assoc (report-vcal-blocks
                                  (get att-cache (:report/message-id r)))
                                 :closed? (boolean (:report/closed r))))
                        events)
        write!    (fn [basename cal-name recs]
                    (let [vevents (dedupe-vevents (mapcat :vevents recs))]
                      (if-let [doc (build-vcalendar
                                    (str source-name " " cal-name)
                                    vevents
                                    (dedupe-vtimezones (mapcat :vtimezones recs)))]
                        (let [filename (str events-dir "/" basename)]
                          (spit filename doc)
                          (log/info "Wrote" (count vevents) "VEVENT(s) to" filename))
                        (delete-stale-file! events-dir basename))))]
    (write! "announcements.ics"        "events"          tagged)
    (write! "announcements-open.ics"   "events (open)"   (remove :closed? tagged))
    (write! "announcements-closed.ics" "events (closed)" (filter :closed? tagged))))

(defn dump-html!
  "Generate index.html for a single source.
  Uses all-open.json so only open reports are server-rendered;
  closed reports are lazy-loaded by the client from all-closed.json."
  [base-dir reports-dir cli-args]
  (let [json-file (str reports-dir "/all-open.json")]
    (apply process/shell "bb" "scripts/bone-index.clj"
           "-o" (str base-dir "/index.html")
           "--json" json-file
           "--dir" reports-dir
           cli-args)))

(defn dump-stats!
  "Generate stats for a single source."
  [base-dir reports-dir source-name format cli-args]
  (let [dir      (if (= format "html") base-dir reports-dir)
        out-file (str dir (if (= format "html") "/data.html" "/stats.json"))]
    (apply process/shell "bb" "scripts/bone-stats.clj"
           (if (= format "html") "html" "json")
           "-o" out-file
           "-n" source-name
           cli-args)))

(defn dump-docs!
  "Generate docs.html for a single source."
  [base-dir source-name cli-args]
  (apply process/shell (cond-> ["bb" "scripts/bone-docs.clj"
                                "-o" (str base-dir "/docs.html")
                                "--dir" base-dir]
                         source-name (into ["-n" source-name])
                         true        (into cli-args))))

;; ---------------------------------------------------------------------------
;; Per-type export
;; ---------------------------------------------------------------------------

(def default-export-formats #{"json" "org" "rss"})

(defn- resolve-export-formats
  "Return the set of export formats for a source.
  Per-source :export-formats in source-map, or the global default."
  [source-name source-map]
  (or (get-in source-map [source-name :export-formats])
      default-export-formats))

;; Export scope: the 5-tuple {:reports :reports-dir :source-name
;; :source-map :maintainers-map} that every dump-* orchestrator
;; consumes.  Built once in `export-source!` and propagated as a map
;; to avoid positional-argument errors.

(defn- with-reports
  "Return scope with :reports swapped (other keys preserved)."
  [scope reports]
  (assoc scope :reports reports))

(defn- dump-typed-formats!
  "Write scope's reports in every format enabled by `fmts`: JSON
  (when `fmts \"json\"` or `:json-always?`), RSS (when `fmts \"rss\"`),
  Org (when `fmts \"org\"`).  `basename` is the file stem (no
  extension); `label` is the human title used in RSS/Org headers."
  [{:keys [reports reports-dir source-name source-map maintainers-map]}
   fmts basename label
   & {:keys [json-always? counts]}]
  (when (or json-always? (fmts "json"))
    (dump-json! reports reports-dir source-name source-map maintainers-map
                (str basename ".json") counts))
  (when (fmts "rss")
    (dump-rss! reports reports-dir source-name source-map maintainers-map
               (str basename ".xml") label))
  (when (fmts "org")
    (dump-org! reports reports-dir source-name source-map maintainers-map
               (str basename ".org") label)))

(defn- dump-per-type!
  "Export per-type JSON, Org, and RSS files for all report types
  present.  When `changed-types` is non-nil, only re-export files for
  those types."
  [{:keys [reports reports-dir] :as scope} fmts & {:keys [changed-types]}]
  (doseq [rtype report-types
          :when (or (nil? changed-types) (changed-types rtype))
          :let  [typed  (filter-reports reports {:type rtype})
                 plural (type->plural rtype)]]
    (if (seq typed)
      (dump-typed-formats! (with-reports scope typed) fmts plural plural)
      (delete-stale-typed! reports-dir plural))))

(defn- dump-open-closed!
  "Export open/closed split files and meta.json with summary counts.
  all-open.json is loaded by index.html on first paint (fast).
  all-closed.json is lazy-loaded when user deactivates the Open filter.
  meta.json contains summary counts per type, used by data.html for KPIs.
  Produces per-type -open and -closed files in all enabled formats.
  When `changed-types` is non-nil, only re-export per-type files for
  those types (aggregate all-open/all-closed and meta.json are always
  regenerated)."
  [{:keys [reports reports-dir source-name source-map] :as scope}
   fmts & {:keys [changed-types]}]
  (let [open        (vec (open-reports reports))
        closed      (vec (filter :report/closed reports))
        counts      {:total        (count reports)
                     :open-count   (count open)
                     :closed-count (count closed)}
        by-type     (group-by :report/type reports)
        type-counts (into {}
                          (map (fn [[t rs]]
                                 [(name t) {:total  (count rs)
                                            :open   (count (remove :report/closed rs))
                                            :closed (count (filter :report/closed rs))}]))
                          by-type)
        json?       (boolean (fmts "json"))
        ;; Authoritative list of JSON files holding reports, so consumers
        ;; (e.g. gnaw --add-source) can tell them apart from meta.json,
        ;; votes.json and stats.json. Mirrors what dump-json!/dump-per-type!/
        ;; dump-open-closed! actually write for this source.
        reports-files (vec (concat
                            (when json? ["all.json"])
                            ;; always written (json-always? in dump-typed-formats!)
                            ["all-open.json" "all-closed.json"]
                            (when json?
                              (for [rtype report-types
                                    :let  [plural   (type->plural rtype)
                                           typed    (get by-type rtype)
                                           t-open   (filter-reports open {:type rtype})
                                           t-closed (filter-reports closed {:type rtype})]
                                    f     (cond-> []
                                            (seq typed)    (conj (str plural ".json"))
                                            (seq t-open)   (conj (str plural "-open.json"))
                                            (seq t-closed) (conj (str plural "-closed.json")))]
                                f))))
        tenures     (when-let [db (ctx-db)] (get-tenures db source-name))
        generated   (str (java.util.Date.))
        meta-data   (merge counts
                           {:bone-format   bone-format
                            :source        source-name
                            :generated     generated
                            :by-type       type-counts
                            :reports-files reports-files
                            ;; JSON file(s) holding the data.html view model
                            ;; (KPIs + chart specs); the data.html shell reads
                            ;; this list to know what to fetch.
                            :stats-files   ["stats.json"]
                            :maintainers   (tenures-snapshot (or tenures []))}
                          (source-metadata source-name source-map))]
    (spit (str reports-dir "/meta.json")
          (json/generate-string meta-data {:pretty true}))
    (log/info "Wrote meta.json")
    ;; Self-contained config.edn so anyone can reproduce this dashboard on
    ;; their own copy of the mail (linked from docs.html under Configuration).
    (when-let [cfg-edn (reproducible-config-str (ctx-config) source-name)]
      (spit (str reports-dir "/config.edn") cfg-edn)
      (log/info "Wrote config.edn"))
    ;; all-open.json carries :generated so the (now data-independent)
    ;; index.html shell can show a freshness timestamp client-side.
    (dump-typed-formats! (with-reports scope open) fmts "all-open" "open reports"
                         :json-always? true :counts (assoc counts :generated generated))
    (dump-typed-formats! (with-reports scope closed) fmts "all-closed" "closed reports"
                         :json-always? true :counts counts)
    (doseq [rtype report-types
            :when (or (nil? changed-types) (changed-types rtype))
            :let  [plural   (type->plural rtype)
                   t-open   (filter-reports open {:type rtype})
                   t-closed (filter-reports closed {:type rtype})]]
      (if (seq t-open)
        (dump-typed-formats! (with-reports scope t-open) fmts
                             (str plural "-open") (str plural " (open)"))
        (delete-stale-typed! reports-dir (str plural "-open")))
      (if (seq t-closed)
        (dump-typed-formats! (with-reports scope t-closed) fmts
                             (str plural "-closed") (str plural " (closed)"))
        (delete-stale-typed! reports-dir (str plural "-closed"))))))

;; ---------------------------------------------------------------------------
;; Root index -- public/index.html listing all sources
;; ---------------------------------------------------------------------------

(defn- load-source-meta
  "Read reports/meta.json for a source dir, or nil on failure."
  [base-dir]
  (let [f (io/file base-dir "reports" "meta.json")]
    (when (.exists f)
      (try (json/parse-string (slurp f) true)
           (catch Exception _ nil)))))

(defn dump-root-index!
  "Generate public/index.html listing all exported sources.
  Reads each source's reports/meta.json for summary counts.
  Feed links honor each source's effective :export-formats so the
  index never points at files the export does not produce."
  [source-names source-map]
  (let [rows (for [src-name source-names
                   :let     [slug     (slugify src-name)
                         base-dir (str "public/" slug)
                         meta     (load-source-meta base-dir)]
                   :when    meta]
               {:name         src-name
                :slug         slug
                :formats      (resolve-export-formats src-name source-map)
                :total        (or (:total meta) 0)
                :open         (or (:open-count meta) 0)
                :closed       (or (:closed-count meta) 0)
                :list-archive (:list-archive meta)})
        row-html
        (fn [{:keys [name slug formats total open closed list-archive]}]
          (let [feeds (cond-> []
                        (formats "rss")
                        (conj (str "<a href=\"" slug "/reports/all.xml\">RSS</a>"))
                        (formats "json")
                        (conj (str "<a href=\"" slug "/reports/all.json\">JSON</a>")))]
            (str "<tr>"
                 "<td><a href=\"" slug "/index.html\">" (xml-escape name) "</a>"
                 (when list-archive
                   (str " <a class=\"archive\" href=\"" (xml-escape list-archive)
                        "\" title=\"List archive\">↗</a>"))
                 "</td>"
                 "<td class=\"num\">" open "</td>"
                 "<td class=\"num\">" closed "</td>"
                 "<td class=\"num\">" total "</td>"
                 "<td class=\"num feeds\">" (str/join " · " feeds) "</td>"
                 "</tr>\n")))
        page
        (str
         "<!DOCTYPE html>\n<html lang=\"en\">\n"
         (html-head {:title "BONE - Sources"
                     :css   (str "table{margin-top:1.5rem}"
                               "td.num,th.num{text-align:right}"
                               "a.archive{font-size:0.82rem;margin-left:0.4rem;opacity:0.7}"
                               ".feeds{font-size:0.82rem;white-space:nowrap}"
                               ".theme-toggle{cursor:pointer;background:none;border:none;font-size:1.2rem;padding:0.3rem}"
                               footer-css)})
         "<body>\n<main class=\"container\">\n"
         (h/html (nav-bar "BONE" nil))
         "\n<table>\n"
         "<thead><tr>"
         "<th>Source</th>"
         "<th class=\"num\">Open</th>"
         "<th class=\"num\">Closed</th>"
         "<th class=\"num\">Total</th>"
         "<th class=\"num\">Feeds</th>"
         "</tr></thead>\n<tbody>\n"
         (apply str (map row-html rows))
         "</tbody></table>\n"
         (h/html (bone-footer {:feeds false}))
         "\n<script>\n" (wrap-js theme-toggle-js) "\n</script>\n"
         "</main>\n</body>\n</html>\n")]
    (spit-html "public/index.html" page)
    (log/info "Wrote public/index.html with" (count rows) "source(s)")))

(defn export-source!
  "Export a single source in the given format(s).
  Always produces all-open.json and all-closed.json (used by index.html).
  When format is \"all\", per-type feeds respect :export-formats from config.
  `changed-types` (optional map {report-type count}, used as a set-like
  predicate) limits per-type file regeneration to those types during
  incremental export; aggregate files are always rebuilt."
  [format reports base-dir source-name source-map maintainers-map cli-extra
   & {:keys [changed-types since regen-shell? regen-docs?]
      :or   {regen-shell? true regen-docs? true}}]
  (let [reports-dir (str base-dir "/reports")
        patches-dir (str base-dir "/patches")
        events-dir  (str base-dir "/events")
        text-dir    (str base-dir "/text")
        _           (doseq [d [reports-dir patches-dir events-dir text-dir]]
                      (.mkdirs (io/file d)))
        ef          (resolve-export-formats source-name source-map)
        scope       {:reports         reports
                     :reports-dir     reports-dir
                     :source-name     source-name
                     :source-map      source-map
                     :maintainers-map maintainers-map}
        do-format
        (fn [fmt]
          (case fmt
            "json"    (do (dump-json! reports reports-dir source-name source-map maintainers-map)
                          (dump-votes! reports reports-dir)
                          (dump-per-type! scope #{"json"})
                          (dump-open-closed! scope #{"json"}))
            "rss"     (do (dump-rss!  reports reports-dir source-name source-map maintainers-map)
                          (dump-per-type! scope #{"rss"}))
            "org"     (do (dump-org!  reports reports-dir source-name source-map maintainers-map)
                          (dump-per-type! scope #{"org"})
                          (dump-open-closed! scope #{"org"}))
            "patches" (dump-patches! reports patches-dir :since since)
            "text"    (dump-text! reports text-dir :since since)
            "events"  (do (dump-events! reports events-dir :since since)
                          (dump-events-filtered! reports reports-dir source-name source-map maintainers-map ef)
                          (dump-events-ics! reports events-dir source-name))
            "html"    (do (dump-json! reports reports-dir source-name source-map maintainers-map)
                          (dump-votes! reports reports-dir)
                          (dump-per-type! scope #{"json"})
                          (dump-open-closed! scope #{"json"})
                          (dump-docs! base-dir source-name cli-extra)
                          (dump-html! base-dir reports-dir cli-extra))
            "stats"   (dump-stats! base-dir reports-dir source-name "json" cli-extra)))]
    (if (= format "all")
      (do (when (ef "json") (dump-json! reports reports-dir source-name source-map maintainers-map))
          ;; Votes are only consumed by JSON and HTML outputs.
          (when (or (ef "json") (ef "html")) (dump-votes! reports reports-dir))
          (when (ef "rss")  (dump-rss!  reports reports-dir source-name source-map maintainers-map))
          (when (ef "org")  (dump-org!  reports reports-dir source-name source-map maintainers-map))
          (dump-per-type! scope ef :changed-types changed-types)
          (dump-open-closed! scope ef :changed-types changed-types)
          (dump-patches! reports patches-dir :since since)
          (dump-text! reports text-dir :since since)
          (dump-events! reports events-dir :since since)
          (dump-events-filtered! reports reports-dir source-name source-map maintainers-map ef)
          (dump-events-ics! reports events-dir source-name)
          ;; stats.json is a data file (always refreshed); data.html, like
          ;; index.html/docs.html, is a data-independent shell skipped on
          ;; data-only runs (the orchestrator preserves it across the swap).
          (dump-stats! base-dir reports-dir source-name "json" cli-extra)
          (when regen-docs?  (dump-docs! base-dir source-name cli-extra))
          (when regen-shell? (dump-html! base-dir reports-dir cli-extra))
          (when regen-shell? (dump-stats! base-dir reports-dir source-name "html" cli-extra)))
      (do-format format))))

;; ---------------------------------------------------------------------------
;; Main
;; ---------------------------------------------------------------------------

(def formats #{"json" "rss" "org" "html" "all" "stats" "patches" "text" "events" "root"})

(let [{:keys [format source-name min-priority min-status force-all? theme page-size closed-retention
              topics-filter html-columns html-columns-sort]
       :or   {format "all"}}
      (parse-cli-args *command-line-args*)
      config (load-config)
      dbp    (db-path config)
      conn   (d/get-conn dbp bone-schema {:wal? false})]
  (try
    (when-not (formats format)
      (log/error "Unknown format:" format)
      (log/error "Formats: json rss org html stats patches text events root all")
      (System/exit 1))
    (when (and min-priority (not (#{1 2 3} min-priority)))
      (log/error "Invalid --min-priority:" min-priority "(must be 1, 2, or 3)")
      (System/exit 1))
    (when (and min-status (not (<= 1 min-status 7)))
      (log/error "Invalid --min-status:" min-status "(must be 1-7)")
      (System/exit 1))
    (let [;; Watermark for save-last-export!: taken *before* the DB snapshot,
          ;; otherwise daemon transactions racing with this export would fall
          ;; before the saved watermark and be skipped forever by the next
          ;; incremental run.
          run-started     (java.util.Date.)
          db              (d/db conn)
          last-modified   (get-last-modified db)
          last-export     (get-last-export)
          config-path     (or (System/getenv "BONE_CONFIG") "config.edn")
          config-mtime    (let [f (io/file config-path)]
                            (when (.exists f) (.lastModified f)))
          config-changed? (and last-export config-mtime
                               (> ^long config-mtime
                                  (.getTime ^java.util.Date last-export)))
          ;; A changed shell asset (template/JS) forces a full re-export so
          ;; the new shells are rebuilt -- even with no report activity, which
          ;; would otherwise skip the run and leave the old HTML in place.
          shell-changed?  (shell-assets-changed? last-export)
          ;; --closed-retention and :awaiting-delay depend on the clock,
          ;; not on DB transactions: reports cross those thresholds with
          ;; no tx to re-trigger their export.  When either is configured,
          ;; force one full run per UTC day so the incremental output
          ;; can't diverge from --force for long.
          day-crossed?    (boolean
                           (and last-export
                                (or closed-retention
                                    (:awaiting-delay config)
                                    (some :awaiting-delay (:sources config)))
                                (not= (format-date-iso last-export)
                                      (format-date-iso run-started))))
          incremental?    (and (not force-all?)
                               (not config-changed?)
                               (not shell-changed?)
                               (not day-crossed?)
                               (= format "all")
                               last-export last-modified)
          skip?           (and incremental?
                               (<= (.getTime ^java.util.Date last-modified)
                                   (.getTime ^java.util.Date last-export)))]
      (when config-changed?
        (log/info "Config changed since last export, forcing full re-export."))
      (when (and shell-changed? (not config-changed?))
        (log/info "Shell assets changed since last export, forcing full re-export."))
      (when (and day-crossed? (not config-changed?) (not shell-changed?) (not force-all?))
        (log/info "Retention/awaiting configured and a day has passed, forcing full re-export."))
      (if skip?
        (log/info "Nothing changed since last export, skipping.")
        ;; Resolve source list *before* the expensive DB pull
        ;; so we can determine which sources actually need re-export.
        (let [effective-theme (or theme (:theme config))
              _               (when effective-theme (set-theme! effective-theme))
              source-map      (if config (build-source-map config) {})
              source-names    (if source-name
                                (if (contains? source-map source-name)
                                  [source-name]
                                  (do (log/error "No source named" (str "'" source-name "'"))
                                      (log/error "Available:"
                                                 (str/join ", " (keys source-map)))
                                      (System/exit 1)))
                                (mapv :name (:sources config)))
              ;; Per-source, per-type change detection: {source -> {type count}}.
              ;; Enables both source-level skip and intra-source per-type skip.
              changed-st      (when (and incremental? last-export)
                                (changed-source-types-since db last-export))
              ;; Split the cron notification's counts: genuine additions
              ;; (new reports, by origin-email date) vs effective report
              ;; modifications (status/flags/relations/expiry), excluding
              ;; mere thread growth.  changed-st still drives re-export.
              new-st          (when (and incremental? last-export)
                                (new-source-types-since db last-export))
              state-st        (when (and incremental? last-export)
                                (state-changed-source-types-since db last-export))
              export-names    (if (and incremental? (seq changed-st))
                                ;; Maintainers-only changes don't bump any
                                ;; report, so they never reach changed-st --
                                ;; without this check the source is skipped
                                ;; and its docs.html stays stale.
                                (filterv (fn [s]
                                           (or (contains? changed-st s)
                                               (maintainers-changed-since? db s last-export)))
                                         source-names)
                                source-names)]
          (when (and incremental? (seq changed-st))
            (log/info "Incremental: changed sources:"
                      (str/join ", " (map (fn [[s ts]] (str s " (" (fmt-type-counts ts) ")"))
                                          changed-st))))
          (when (and incremental? (seq changed-st) (< (count export-names) (count source-names)))
            (let [skipped (remove (set export-names) source-names)]
              (doseq [s skipped]
                (log/info (str "[" s "]") "no changes, skipping."))))
          ;; Only load all reports and votes when there is actual work to do.
          ;; Collect the source names actually exported so we can skip the
          ;; root index when nothing changed and notify on the cron mail.
          (let [exported-srcs
                (if (and (not= format "root") (seq export-names))
                  (let [maintainers-map (if config (build-maintainers db source-map) {})
                        all-reps        (all-reports-by-date db)
                        votes           (votes-by-report
                                         (d/q '[:find ?r ?val ?voter ?emid ?hdrs
                                                :where
                                                [?v :vote/report ?r]
                                                [?v :vote/value  ?val]
                                                [?v :vote/voter  ?voter]
                                                [?v :vote/email  ?e]
                                                [?e :email/message-id ?emid]
                                                [(get-else $ ?e :email/headers-edn "") ?hdrs]]
                                              db))
                        cli-tf          (resolve-topics-filter topics-filter)
                        drop-cutoff     (resolve-closed-retention closed-retention)
                        _               (when cli-tf
                                          (log/info "CLI topics filter:" (str/join ", " cli-tf)))
                        _               (when drop-cutoff
                                          (log/info "Dropping reports closed before" drop-cutoff))
                        ;; Flags forwarded to the index/stats/docs
                        ;; sub-scripts: an explicit whitelist rebuilt from
                        ;; the parsed flags (raw-token subtraction used to
                        ;; strip flags whose value collided, e.g. with the
                        ;; format name).
                        cli-extra       (cond-> []
                                          effective-theme   (into ["--html-theme" effective-theme])
                                          page-size         (into ["--html-page-size" (str page-size)])
                                          html-columns      (into ["--html-columns" html-columns])
                                          html-columns-sort (into ["--html-columns-sort" html-columns-sort]))]
                    (binding [*export-ctx* (build-export-ctx db votes config)]
                      (reduce (fn [exported src-name]
                                (let [reports     (filter-reports all-reps {:source       src-name
                                                                            :min-priority min-priority
                                                                            :min-status   min-status})
                                      rt          (get-in source-map [src-name :report-types])
                                      reports     (if rt (filter #(contains? rt (:report/type %)) reports) reports)
                                      reports     (if drop-cutoff (drop-old-closed reports drop-cutoff) reports)
                                      reports     (filter-by-topics reports cli-tf)
                                      slug        (slugify src-name)
                                      staging     (str "public/.staging-" slug)
                                      final-dir   (str "public/" slug)
                                      exported-before? (.exists (io/file final-dir))
                                      src-changed (when (seq changed-st) (get changed-st src-name))
                                      ;; HTML shells: a full run (incremental? false,
                                      ;; which also covers config/asset changes) rebuilds
                                      ;; both.  On an incremental run we rebuild only when
                                      ;; the file is missing or -- for docs -- maintainers
                                      ;; changed; otherwise the shells stay byte-stable.
                                      ;; regen-shell? also governs data.html
                                      ;; (dump-stats! "html"), so check both.
                                      regen-shell? (or (not incremental?)
                                                       (not (.exists (io/file final-dir "index.html")))
                                                       (not (.exists (io/file final-dir "data.html"))))
                                      regen-docs?  (or (not incremental?)
                                                       (maintainers-changed-since? db src-name last-export)
                                                       (not (.exists (io/file final-dir "docs.html"))))]
                                  (if (and (empty? reports) (not exported-before?))
                                    (do
                                      (log/info "No reports for source" (str "'" src-name "'")
                                                "and no previous export, skipping.")
                                      exported)
                                    (do
                                      (log/info (str "[" src-name "] " (count reports) " report(s)"
                                                     " (" (count (open-reports reports)) " open)"
                                                     (if incremental? " (incremental)" "")))
                                      (try
                                        (delete-dir! (io/file staging))
                                        ;; Single-format runs: FULL copy of final-dir,
                                        ;; or the swap would wipe the other formats.
                                        ;; "all": incremental seeds only the subdirs it
                                        ;; does not rewrite; a full run starts from an
                                        ;; empty staging so stale files are purged.
                                        (if (not= format "all")
                                          (copy-dir! (io/file final-dir)
                                                     (io/file staging))
                                          (when incremental?
                                            (doseq [sub ["reports" "patches" "text" "events"]]
                                              (copy-dir! (io/file final-dir sub)
                                                         (io/file staging sub)))))
                                        ;; Preserve the previous HTML shells when not
                                        ;; regenerating them: they are top-level files
                                        ;; not covered by the subdir seed above, and
                                        ;; would vanish in the atomic swap.
                                        (preserve-shell! regen-shell? final-dir staging "index.html")
                                        (preserve-shell! regen-shell? final-dir staging "data.html")
                                        (preserve-shell! regen-docs?  final-dir staging "docs.html")
                                        (export-source! format reports staging src-name
                                                        source-map maintainers-map cli-extra
                                                        :changed-types src-changed
                                                        :since (when incremental? last-export)
                                                        :regen-shell? regen-shell?
                                                        :regen-docs? regen-docs?)
                                        (atomic-swap-dir! staging final-dir)
                                        (catch Exception e
                                          (log/error e "Export failed for" src-name "-- cleaning up staging dir")
                                          (delete-dir! (io/file staging))
                                          (throw e)))
                                      (conj exported src-name)))))
                              [] export-names)))
                  [])]
            ;; Only regenerate root index when at least one source was exported,
            ;; or when explicitly requested via "bb export root".
            ;; Always list every configured source: source-names is narrowed
            ;; to one entry by -n, and the root index must not shrink to it
            ;; (the :when meta filter drops never-exported sources anyway).
            (when (and (#{"all" "root"} format)
                       (or (= format "root") (seq exported-srcs)))
              (dump-root-index! (mapv :name (:sources config)) source-map))
            ;; Cron notification: one stderr line iff real work was
            ;; published ("Wrote ..." progress goes to stdout).  Names
            ;; the changed report types per source (pre-filter counts);
            ;; full re-exports are flagged; runs triggered solely by
            ;; thread replies stay silent.
            (let [summaries (into {}
                                  (keep (fn [s]
                                          (when-let [sm (fmt-change-summary
                                                         (get new-st s) (get state-st s))]
                                            [s sm])))
                                  exported-srcs)]
              (when (and (seq exported-srcs)
                         (or (not incremental?) (seq summaries)))
                (binding [*out* *err*]
                  (println
                   (str "bone: exported "
                        (str/join ", "
                                  (map (fn [s]
                                         (if-let [summary (get summaries s)]
                                           (str s " (" summary ")")
                                           s))
                                       exported-srcs))
                        (when-not incremental? " [full re-export]")))))))
          ;; Advance the incremental watermark only after a full,
          ;; unfiltered run: a partial run (single format, -n, or a
          ;; priority/status/topics filter) does not publish everything,
          ;; so moving the watermark would hide those changes from the
          ;; next incremental export.  --closed-retention is fine: it
          ;; only drops old closed reports, and day-crossed? forces a
          ;; daily full run to keep retention output converged.
          (when (and (= format "all")
                     (nil? source-name)
                     (nil? min-priority)
                     (nil? min-status)
                     (nil? topics-filter))
            (save-last-export! run-started)))))
    (finally
      (d/close conn))))
