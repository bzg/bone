#!/usr/bin/env bb

;; bone.clj — Browse BARK reports interactively.
;;
;; Standalone CLI tool: reads JSON produced by bark export from a file,
;; a URL, stdin, or a URLs file listing multiple report sources.
;; Displays via fzf (with detail on selection) or plain text fallback.
;;
;; Configuration (~/.config/bone/config.edn):
;;   {:my-addresses ["you@example.com" "alias@example.com"]}
;;
;; Usage:
;;   bone.clj [options]
;;   bone.clj clear           Empty the cache
;;   bone.clj update          Fetch/update reports from all sources
;;   bone.clj report          Print a triage summary for maintainers
;;
;; Options:
;;   -f, --file FILE          Read reports from a JSON file
;;   -u, --url  URL           Fetch reports from a URL
;;   -U, --urls-file FILE     Fetch & merge reports from URLs listed in FILE
;;   -M, --my-addresses EMAILS Your email(s), comma-separated (overrides config)
;;   -p, --min-priority 1-3   Only show reports with priority >= N
;;   -s, --min-score 0-7     Only show reports with status score >= N
;;   -n, --source NAME        Filter by source name
;;   -S, --skip-columns COLS  Columns to hide, comma-separated
;;   -m, --mine               Show only reports involving your address(es)
;;   -c, --closed             Include closed reports
;;   -a, --add-source PATH    Add a reports.json source (URL or path)
;;   -r, --remove-source PATH Remove a reports.json source
;;   -l, --list-sources       List configured sources
;;   -h, --help               Show help
;;   -                        Read JSON from stdin
;;
;; By default, bone shows all open reports from configured sources.

(ns bzg.bone
  (:require [babashka.process :as process]
            [babashka.http-client :as http]
            [babashka.fs :as fs]
            [cheshire.core :as json]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [clojure.java.io :as io]))

;; ---------------------------------------------------------------------------
;; Config
;; ---------------------------------------------------------------------------

(def config-path
  (str (System/getProperty "user.home") "/.config/bone/config.edn"))

(def cache-dir
  (str (System/getProperty "user.home") "/.config/bone/cache"))

(def patches-cache-dir
  (str cache-dir "/patches"))

(def events-cache-dir
  (str cache-dir "/events"))

(def texts-cache-dir
  (str cache-dir "/texts"))

(def reports-cache-dir
  (str cache-dir "/reports"))

(defn- load-config []
  (let [f (io/file config-path)]
    (if (.exists f)
      (try
        (edn/read-string (slurp f))
        (catch Exception _
          (throw (ex-info (str "Config file is ill-formed: " config-path) {}))))
      {})))

(defn- load-sources
  "Load :sources from config.edn — a vector of {:url ... :repo ...} maps."
  []
  (vec (:sources (load-config))))

(defn- save-sources! [sources]
  (let [config  (load-config)
        deduped (->> sources (reduce (fn [m s] (assoc m (:url s) s)) {}) vals vec)
        updated (assoc config :sources deduped)]
    (.mkdirs (.getParentFile (io/file config-path)))
    (spit config-path (pr-str updated))))

(defn- add-source! [url]
  (let [sources (load-sources)]
    (if (some #(= (:url %) url) sources)
      (println (str "Already registered: " url))
      (do (save-sources! (conj sources {:url url}))
          (println (str "Added: " url))))))

(defn- remove-source! [url]
  (let [sources (load-sources)]
    (if (some #(= (:url %) url) sources)
      (do (save-sources! (remove #(= (:url %) url) sources))
          (println (str "Removed: " url)))
      (println (str "Not found: " url)))))

(defn- list-sources! []
  (let [sources (load-sources)]
    (if (empty? sources)
      (println "No sources configured. Use --add-source URL_OR_PATH to add one.")
      (doseq [{:keys [url repo]} sources]
        (println (str "  " url (when repo (str "  (repo: " repo ")"))))))))

;; ---------------------------------------------------------------------------
;; Data loading
;; ---------------------------------------------------------------------------

(def min-bark-format "0.6.0")

(defn- version-< [a b]
  (loop [as (str/split a #"\.")
         bs (str/split b #"\.")]
    (let [ai (parse-long (or (first as) "0"))
          bi (parse-long (or (first bs) "0"))]
      (cond
        (< ai bi) true
        (> ai bi) false
        (and (empty? (rest as)) (empty? (rest bs))) false
        :else (recur (rest as) (rest bs))))))

(defn- load-json-string [s]
  (json/parse-string s keyword))

(defn- url? [s]
  (boolean (re-find #"^https?://" s)))

(defn- strip-file-scheme
  "Strip file:// prefix if present."
  [s]
  (cond-> s (str/starts-with? s "file://") (str/replace-first "file://" "")))

(defn- source-base-dir
  "Compute the base directory from a source path/URL.
  For file paths, returns the parent directory.
  For HTTP URLs, returns nil (use base-url instead)."
  [src]
  (when-not (url? src)
    (when-let [parent (.getParent (io/file (strip-file-scheme src)))]
      (str parent "/"))))

(defn- source-base-url
  "Derive a base URL from an HTTP source URL.  BARK serves the JSON from a
  `reports/` subdirectory while attachments (patches/, events/, texts/) are
  siblings of `reports/`.  So we strip the filename, then strip `reports/`
  when present.
  E.g. \"https://example.org/tracker/reports/all.json\" => \"https://example.org/tracker/\"
       \"https://example.org/data.json\"                => \"https://example.org/\"
  Returns nil for non-HTTP sources."
  [src]
  (when (url? src)
    (when-let [i (str/last-index-of src "/")]
      (let [dir (subs src 0 (inc i))]
        (if (str/ends-with? dir "/reports/")
          (subs dir 0 (- (count dir) (count "reports/")))
          dir)))))

(defn- fetch-source-body
  "Fetch raw JSON body string from a source (URL or file path)."
  [src]
  (if (url? src)
    (let [resp (http/get src {:headers {"Accept" "application/json"}})]
      (when (not= 200 (:status resp))
        (throw (ex-info (str "HTTP " (:status resp)) {:url src :status (:status resp)})))
      (:body resp))
    (slurp (strip-file-scheme src))))

(defn- unwrap-envelope
  "Unwrap a reports.json envelope. Returns {:reports [...]}.
  Injects :source and :base-url from envelope into each report.
  Handles: new per-source format {source, reports, list-id, ...}
  and legacy bare-array format."
  [data]
  (if (sequential? data)
    {:reports data}
    (let [fv (:bark-format data)]
      (when (and fv (version-< fv min-bark-format))
        (binding [*out* *err*]
          (println (str "Warning: bark-format " fv
                        " is older than minimum supported version "
                        min-bark-format))))
      (let [reports   (or (:reports data) [])
            src-name  (:source data)
            base-url (:base-url data)]
        {:reports (mapv (fn [r]
                          (cond-> r
                            (and src-name (not (:source r))) (assoc :source src-name)
                            base-url                         (assoc :base-url base-url)))
                        reports)}))))

(defn- inject-base-dir
  "Inject :base-dir into each report from a source path."
  [result src]
  (if-let [base (source-base-dir src)]
    (update result :reports (fn [rs] (mapv #(assoc % :base-dir base) rs)))
    result))

(defn- inject-base-url
  "Inject :base-url into each report from a source URL, when not already set."
  [result src]
  (if-let [base (source-base-url src)]
    (update result :reports (fn [rs] (mapv #(if (:base-url %) % (assoc % :base-url base)) rs)))
    result))

(defn- load-from-file [path]
  (when-not (.exists (io/file path))
    (throw (ex-info (str "File not found: " path) {:path path})))
  (inject-base-dir (unwrap-envelope (load-json-string (slurp path))) path))

(defn- load-from-url [url]
  (inject-base-url (unwrap-envelope (load-json-string (fetch-source-body url))) url))

(defn- load-from-stdin []
  (unwrap-envelope (load-json-string (slurp *in*))))

(defn- merge-results
  "Reduce a seq of {:reports [...]} into one."
  [results]
  {:reports (into [] (mapcat :reports) results)})

(defn- load-from-urls-file [path]
  (when-not (.exists (io/file path))
    (throw (ex-info (str "URLs file not found: " path) {:path path})))
  (let [urls (->> (str/split-lines (slurp path))
                  (map str/trim)
                  (remove #(or (str/blank? %) (str/starts-with? % "#"))))]
    (merge-results (map load-from-url urls))))

;; --- Cache ---

(defn- source->cache-file
  "Deterministic cache filename for a source URL/path."
  [src]
  (let [h (format "%08x" (hash src))
        safe (str/replace src #"[^a-zA-Z0-9._-]" "_")
        prefix (subs safe 0 (min 80 (count safe)))]
    (str reports-cache-dir "/" prefix "-" h ".json")))

(defn- cache-read
  "Read cached reports.json for a source. Returns nil if not cached."
  [src]
  (let [f (io/file (source->cache-file src))]
    (when (.exists f)
      (load-json-string (slurp f)))))

(defn- cache-write!
  "Write raw JSON string to cache for a source."
  [src body]
  (let [f (io/file (source->cache-file src))]
    (.mkdirs (.getParentFile f))
    (spit f body)))

(defn- clear-cache! []
  (when (fs/exists? cache-dir)
    (fs/delete-tree cache-dir))
  (println "Cache cleared."))

(defn- load-one-source-cached
  "Load reports from cache if available, otherwise fetch and cache."
  [src]
  (let [data (or (cache-read src)
                 (let [body (fetch-source-body src)]
                   (cache-write! src body)
                   (load-json-string body)))]
    (-> (unwrap-envelope data)
        (inject-base-dir src)
        (inject-base-url src))))

(defn- update-sources-cache!
  "Fetch all configured sources and update cache."
  []
  (let [sources (load-sources)]
    (when (empty? sources)
      (throw (ex-info "No sources configured." {})))
    (doseq [{:keys [url]} sources]
      (try
        (cache-write! url (fetch-source-body url))
        (println (str "  Updated: " url))
        (catch Exception e
          (binding [*out* *err*]
            (println (str "  [warn] Failed to update " url ": " (.getMessage e)))))))))

(defn- load-from-sources
  "Load and merge reports from all configured sources."
  []
  (let [sources (load-sources)]
    (when (empty? sources)
      (throw (ex-info (str "No sources configured and no -f/-u/-U/- given.\n"
                           "Add a source:  bone --add-source URL_OR_PATH\n"
                           "Or use:        bone -f FILE | -u URL | -")
                      {})))
    (merge-results
     (keep (fn [{:keys [url]}]
             (try
               (load-one-source-cached url)
               (catch Exception e
                 (binding [*out* *err*]
                   (println (str "  [warn] Failed to load " url ": " (.getMessage e))))
                 nil)))
           sources))))

;; ---------------------------------------------------------------------------
;; Filtering
;; ---------------------------------------------------------------------------

(defn- normalize-addresses
  "Coerce :my-addresses (a string or vector of strings) into a set of
  lower-cased addresses."
  [v]
  (when v
    (let [xs (if (string? v) [v] v)]
      (into #{} (map str/lower-case) xs))))

(defn- involves-email?
  "True when the report involves any of the given email addresses."
  [report addresses]
  (let [addrs (if (set? addresses) addresses (normalize-addresses addresses))]
    (some #(when % (contains? addrs (str/lower-case %)))
          [(:from report)
           (:acked report) (:owned report) (:closed report)
           (:acked-proxy report) (:owned-proxy report) (:closed-proxy report)])))


;; ---------------------------------------------------------------------------
;; Formatting helpers
;; ---------------------------------------------------------------------------

(defn- multiple-sources? [reports]
  (> (count (distinct (keep :source reports))) 1))

(defn- date-only
  "Extract just the date portion, stripping any leading weekday and trailing time.
  Handles both 'Sat Mar 07 14:30' and '2026-03-07 14:30' style dates."
  [s]
  (if (and s (seq s))
    (let [s (str/trim s)
          ;; Strip leading 'Mon ' / 'Tue ' etc.
          s (str/replace-first s #"^[A-Z][a-z]{2}\s+" "")
          ;; Strip time portion after date (either 'HH:MM...' or 'THH:MM...')
          s (str/replace-first s #"[T ]?\d{2}:\d{2}.*" "")]
      (str/trim s))
    ""))

(defn- parse-votes
  "Parse a votes string like \"3/5\" (sum/total) into [sum total].
  Returns [0 0] on nil."
  [v]
  (if (and v (re-matches #"-?\d+/\d+" v))
    (mapv parse-long (str/split v #"/"))
    [0 0]))

(defn- vote-cookie
  "Format a vote cookie like \"[3/5] \" from a report's :votes field.
  Returns empty string when no votes."
  [report]
  (if-let [v (:votes report)]
    (str "[" v "] ")
    ""))

(defn- days-until
  "Days from now to the report's date field (yyyy-mm-dd).
  Negative = past. Returns nil when field is absent."
  [report field]
  (when-let [d (get report field)]
    (try
      (.between java.time.temporal.ChronoUnit/DAYS
                (java.time.LocalDate/now)
                (java.time.LocalDate/parse d))
      (catch Exception _ nil))))

(defn- deadline-col
  "Format the deadline column: days as string, or empty."
  [report]
  (if-let [d (days-until report :deadline)] (str d) ""))

(defn- display-type
  "Map a BARK report type to its short display form."
  [t]
  (case t "announcement" "announce" t))

(defn- truncate
  "Truncate string s to at most n characters."
  [s n]
  (if (> (count s) n) (subs s 0 n) s))

(defn- report-flags+score
  "Compute flags string and numeric score from report fields.
  Flags: A=acked O=owned, third char: C=canceled R=resolved E=expired -=open.
  Score matches bark-index.clj: acked=1, owned=2, open=4 (closed=0)."
  [report]
  (let [a? (:acked report)
        o? (:owned report)
        c? (:closed report)
        cr (:close-reason report)]
    {:flags (str (if a? "A" "-")
                 (if o? "O" "-")
                 (case cr
                   "canceled" "C"
                   "resolved" "R"
                   "expired"  "E"
                   (if c? "R" "-")))
     :score (+ (if a? 1 0) (if o? 2 0) (if c? 0 4))}))

(def ^:private column-aliases
  "Short aliases for column names."
  {"p" "priority", "d" "deadline", "#" "replies"})

(defn- normalize-column [col]
  (get column-aliases col col))

(defn- normalize-skip-columns [skip]
  (set (map normalize-column (or skip #{}))))

(defn- report-columns
  "Return a vector of column values for a report.
  skip is a set of column names to hide (e.g. #{\"owner\" \"att\"})."
  [report show-type? show-src? skip]
  (let [skip (normalize-skip-columns skip)]
    (concat
     (when (and show-type? (not (skip "type")))   [(display-type (:type report ""))])
     (when (and show-src?  (not (skip "source"))) [(truncate (:source report "") 10)])
     (when-not (skip "priority") [(case (:priority report 0) 3 "A" 2 "B" 1 "C" " ")])
     (when-not (skip "deadline") [(deadline-col report)])
     (when-not (skip "flags")    [(:flags (report-flags+score report))])
     (when-not (skip "replies")  [(str (:replies report 0))])
     (when-not (skip "author")   [(truncate (:from report "?") 15)])
     (when-not (skip "owner")    [(truncate (:owned report "") 15)])
     (when-not (skip "date")     [(date-only (:date report))])
     (when-not (skip "att")      [(str (when (seq (:patches report)) "+")
                                       (when (seq (:events report))  "@")
                                       (when (seq (:texts report))   "#")
                                       (when (:awaiting report)      "?")
                                       (when (seq (:related report)) "~"))])
     [(str (vote-cookie report) (:subject report "(no subject)"))])))

(defn- report->row
  "Format a report as a tab-separated row for fzf display."
  [report show-type? show-src? skip]
  (str/join "\t" (report-columns report show-type? show-src? skip)))

(defn- extra-str [report]
  (->> [(:source report)
        (:version report)
        (:topic report)
        (:patch-seq report)
        (when-let [sources (seq (:patch-source report))]
          (str "src:" (str/join "," sources)))
        (when-let [ps (seq (:patches report))]
          (str "📎" (count ps)))
        (when-let [s (:series report)]
          (str "series:" (:received s) "/" (:expected s)
               (when (:closed s) " closed")))
        (when-let [related (seq (:related report))]
          (str "→" (str/join "," (distinct (map (comp display-type :type) related)))))]
       (keep identity)
       seq
       (str/join " ")))

(defn- report->line
  "Format a report as a plain text line."
  [report show-type? show-src? skip]
  (let [line (str/join "  " (report-columns report show-type? show-src? skip))]
    (if-let [e (extra-str report)]
      (str line " " e)
      line)))

;; ---------------------------------------------------------------------------
;; Display
;; ---------------------------------------------------------------------------

(defn- fzf-available? []
  (try
    (zero? (:exit (process/shell {:out :string :err :string :continue true} "fzf" "--version")))
    (catch Exception _ false)))

(defn- tabulate
  "Align tab-separated rows into fixed-width columns."
  [rows]
  (-> (process/shell {:in (str/join "\n" rows) :out :string}
                     "column" "-t" "-s" "\t")
      :out
      str/trim
      str/split-lines))

(defn- command-available? [cmd]
  (try
    (zero? (:exit (process/shell {:out :string :err :string :continue true} "which" cmd)))
    (catch Exception _ false)))

(def ^:private text-browser-cmds
  [["w3m"   ["w3m" "-o" "confirm_qq=false"]]
   ["lynx"  ["lynx"]]
   ["links" ["links"]]])

(defn- platform-opener []
  (let [os (str/lower-case (System/getProperty "os.name"))]
    (cond
      (str/includes? os "mac") ["open"]
      (str/includes? os "win") ["start"]
      :else                    ["xdg-open"])))

(defn- browse-cmd
  "Return the best available command for viewing a URL.
  Honors :text-browser from config, then probes w3m/lynx/links,
  falls back to platform opener."
  [config]
  (or (when-let [b (:text-browser config)]
        (some (fn [[cmd v]] (when (= cmd b) v)) text-browser-cmds))
      (some (fn [[cmd v]] (when (command-available? cmd) v))
            text-browser-cmds)
      (platform-opener)))

(defn- attachment-paths
  "Return a vector of {:url ... :cache-path ...} for attachments of a given kind.
  `kind` is the report key (:events or :texts), `subdir` the URL/cache subdirectory."
  [report kind subdir cache-dir]
  (when-let [atts (seq (get report kind))]
    (let [base       (or (:base-dir report)
                         (when-let [bp (:base-url report)]
                           (if (str/ends-with? bp "/") bp (str bp "/"))))
          src-name   (or (:source report) "default")]
      (mapv (fn [a]
              (let [file       (:file a)
                    cache-file (str/replace-first file #"^(\.\./?)+" "")]
                {:url        (if base (str base subdir "/" file) file)
                 :cache-path (str cache-dir "/" src-name "/" cache-file)}))
            atts))))

(defn- patch-paths
  "Return a vector of {:url ... :cache-path ... :filename ...} for each patch in a report.
  Delegates to attachment-paths, then adds :filename from patch metadata."
  [report]
  (when-let [paths (attachment-paths report :patches "patches" patches-cache-dir)]
    (mapv (fn [path patch]
            (assoc path :filename (:patch/filename patch (:file patch))))
          paths (:patches report))))

(defn- event-paths [report] (attachment-paths report :events "events" events-cache-dir))
(defn- text-paths  [report] (attachment-paths report :texts  "text"  texts-cache-dir))

(def ^:private diff-pager-cmds
  [["delta"          ["delta" "--paging" "always"]]
   ["bat"            ["bat" "--style=plain" "--language=diff" "--paging=always"]]
   ["diff-so-fancy"  ["diff-so-fancy"]]])

(def ^:private stdin-diff-pagers
  "Pagers that read from stdin rather than a file argument."
  #{"delta" "diff-so-fancy"})

(defn- patch-pager
  "Return a command vector for viewing patches with syntax highlighting.
  Honors :diff-pager from config, then probes delta/bat, falls back to $PAGER/less."
  [config]
  (or (when-let [p (:diff-pager config)]
        (some (fn [[cmd v]] (when (= cmd p) v)) diff-pager-cmds))
      (some (fn [[cmd v]] (when (and (not= cmd "diff-so-fancy") (command-available? cmd)) v))
            diff-pager-cmds)
      [(or (System/getenv "PAGER") "less")]))

;; ---------------------------------------------------------------------------
;; Sorting
;; ---------------------------------------------------------------------------

(defn- parse-date-ms
  "Parse a date-raw string to epoch millis for sorting. Returns 0 on failure."
  [s]
  (if (or (nil? s) (str/blank? s))
    0
    (or (try (.getTime (.parse (java.text.SimpleDateFormat.
                                "EEE MMM dd HH:mm:ss z yyyy"
                                java.util.Locale/ENGLISH) s))
             (catch Exception _ nil))
        (try (.toEpochMilli (java.time.Instant/parse s))
             (catch Exception _ nil))
        (try (-> (java.time.LocalDate/parse s)
                 (.atStartOfDay java.time.ZoneOffset/UTC)
                 .toInstant
                 .toEpochMilli)
             (catch Exception _ nil))
        0)))

(def sort-options
  [["date (newest)"    (fn [r] (- (parse-date-ms (:date-raw r))))               compare]
   ["date (oldest)"    (fn [r] (parse-date-ms (:date-raw r)))                   compare]
   ["priority (high)"  (fn [r] (- (:priority r 0)))                              compare]
   ["status (active)"  (fn [r] (- (:score (report-flags+score r))))              compare]
   ["deadline (closest)"  (fn [r] (or (days-until r :deadline) Integer/MAX_VALUE))          compare]
   ["deadline (farthest)" (fn [r] (- (or (days-until r :deadline) Integer/MIN_VALUE)))       compare]
   ["replies (most)"   (fn [r] (- (:replies r 0)))                               compare]
   ["votes"            (fn [r] (let [[sum total] (parse-votes (:votes r))]
                                 (if (pos? total) (- (/ (double sum) total)) 0.0)))
    compare]
   ["activity (recent)" (fn [r] (- (parse-date-ms (:last-activity r))))           compare]
   ["activity (oldest)" (fn [r] (parse-date-ms (:last-activity r)))               compare]
   ["awaiting (recent)" (fn [r] (if (:awaiting r) (- (parse-date-ms (:date-raw r))) Long/MAX_VALUE)) compare]
   ["awaiting (oldest)" (fn [r] (if (:awaiting r) (parse-date-ms (:date-raw r)) Long/MAX_VALUE))     compare]
   ["type"             (fn [r] (display-type (:type r "")))                       compare]])

(defn- pick-sort!
  "Let user pick a sort order via fzf. Returns index or nil."
  []
  (let [labels (mapv first sort-options)
        input  (str/join "\n" labels)
        {:keys [exit out]}
        (process/shell {:in input :out :string :continue true}
                       "fzf" "--prompt" "sort by> " "--no-sort" "--reverse")]
    (when (zero? exit)
      (let [selected (str/trim out)
            idx (.indexOf ^java.util.List labels selected)]
        (when (>= idx 0) idx)))))

(defn- sort-reports [reports sort-idx]
  (let [[_ key-fn cmp] (nth sort-options sort-idx)]
    (sort-by key-fn cmp reports)))

(defn- pick-multi!
  "Let user pick from `all` via fzf multi-select.
  '[all]' resets to full set. Returns a set, or nil on cancel."
  [prompt all]
  (let [input (str/join "\n" (cons "[all]" all))
        {:keys [exit out]}
        (process/shell {:in input :out :string :continue true}
                       "fzf" "--multi" "--prompt" prompt
                       "--no-sort" "--reverse")]
    (when (zero? exit)
      (let [selected (set (remove str/blank? (str/split-lines (str/trim out))))]
        (if (contains? selected "[all]")
          (set all)
          selected)))))

;; ---------------------------------------------------------------------------
;; Dispatch script for fzf execute bindings
;; ---------------------------------------------------------------------------

(defn- shell-escape
  "Escape a string for use inside single quotes in shell."
  [s]
  (str "'" (str/replace s "'" "'\\''") "'"))

(def ^:private usage-text
  (str/join "\n"
            ["  Ctrl-n                     Move to the next line"
             "  Ctrl-p                     Move to the previous line"
             "  Enter                      View report in terminal browser"
             "  Ctrl-o                     Open report in system browser"
             "  Ctrl-v                     View attachment (patch, ics, txt)"
             "  Ctrl-/                     Show related reports"
             "  Ctrl-s                     Change sort order"
             "  Ctrl-b                     Filter by source"
             "  Ctrl-r                     Filter by report type"
             "  Ctrl-t                     Filter by topic"
             "  Ctrl-x                     Remove all filters"
             "  Ctrl-u                     Update cache and reload"
             "  Ctrl-h                     Show this help"]))

(defn- page-cmd-str
  "Return a shell command string to page a file, given pager config."
  [pager stdin? dsf? file-var]
  (if stdin?
    (if dsf?
      (str "cat " file-var " | diff-so-fancy | less -RFX")
      (str (str/join " " (map shell-escape pager)) " < " file-var))
    (str (str/join " " (map shell-escape pager)) " " file-var)))

(defn- write-dispatch-script!
  "Write a temp shell script that maps fzf line numbers to actions.
  The script takes two args: ACTION (open|view|browse) and LINE_NUMBER.
  Attachments are fetched lazily on first view."
  [dispatch-path config visible]
  (let [browse  (browse-cmd config)
        pager   (patch-pager config)
        stdin?  (stdin-diff-pagers (first pager))
        dsf?    (= "diff-so-fancy" (first pager))
        plain-pager (or (System/getenv "PAGER") "less")
        sb      (StringBuilder.)]
    (.append sb "#!/bin/sh\nACTION=\"$1\"\nN=\"$2\"\n")
    ;; Lazy fetch helper: bone_fetch URL CACHE_PATH
    (.append sb (str/join "\n"
                          ["bone_fetch() {"
                           "  local url=\"$1\" dest=\"$2\""
                           "  if [ ! -f \"$dest\" ]; then"
                           "    mkdir -p \"$(dirname \"$dest\")\""
                           "    case \"$url\" in"
                           "      http://*|https://*) curl -sfLo \"$dest\" \"$url\" || wget -qO \"$dest\" \"$url\" ;;"
                           "      *) cp \"$url\" \"$dest\" ;;"
                           "    esac"
                           "  fi"
                           "}"
                           ""]))
    (.append sb (str "if [ \"$ACTION\" = \"help\" ]; then\n"
                     "  cat <<'BONE_HELP' | less -RX\n"
                     usage-text "\n"
                     "BONE_HELP\n"
                     "  exit 0\n"
                     "fi\n"))
    (.append sb "case \"$ACTION:$N\" in\n")
    (doseq [[n report] (map-indexed vector visible)]
      (let [url  (:archived-at report)
            ps   (patch-paths report)
            es   (event-paths report)
            ts   (text-paths report)]
        ;; open action (text browser / enter)
        (when url
          (.append sb (str "  open:" n ")\n"))
          (.append sb (str "    " (str/join " " (map shell-escape (conj browse url))) "\n"))
          (.append sb "    ;;\n"))
        ;; browse action (system browser / ctrl-o)
        (when url
          (.append sb (str "  browse:" n ")\n"))
          (let [opener (platform-opener)]
            (.append sb (str "    " (str/join " " (map shell-escape (conj opener url))) "\n")))
          (.append sb "    ;;\n"))
        ;; view action — view any attachment (patch, ics, txt)
        (let [groups (cond-> []
                       (seq ps) (conj {:label "patch" :items ps :diff? true})
                       (seq es) (conj {:label "event" :items es :diff? false})
                       (seq ts) (conj {:label " text" :items ts :diff? false}))]
          (when (seq groups)
            (.append sb (str "  view:" n ")\n"))
            (let [emit-fetch-and-page
                  (fn [{:keys [url cache-path]} diff?]
                    (str "    bone_fetch " (shell-escape url) " " (shell-escape cache-path) "\n"
                         "    " (if diff?
                                 (page-cmd-str pager stdin? dsf? (shell-escape cache-path))
                                 (str (shell-escape plain-pager) " " (shell-escape cache-path)))
                         "\n"))
]
              (if (and (= 1 (count groups)) (= 1 (count (:items (first groups)))))
                ;; Single attachment — no picker needed
                (emit-fetch-and-page (first (:items (first groups))) (:diff? (first groups)))
                ;; Flatten all attachments into one picker with "label: filename" entries
                (let [entries (for [{:keys [label items diff?]} groups
                                    item items]
                                {:label (str label ": " (last (str/split (:url item) #"/")))
                                 :item item
                                 :diff? diff?})
                      entry-labels (mapv :label entries)]
                  (.append sb (str "    PICK=$(printf "
                                   (shell-escape (str/join "\\n" entry-labels))
                                   " | fzf --prompt 'view> ' --no-sort --reverse)\n"))
                  (.append sb "    [ -z \"$PICK\" ] && exit 0\n")
                  (.append sb "    case \"$PICK\" in\n")
                  (doseq [{:keys [label item diff?]} entries]
                    (.append sb (str "      " (shell-escape label) ")\n"))
                    (.append sb (str "    " (emit-fetch-and-page item diff?)))
                    (.append sb "        ;;\n"))
                  (.append sb "    esac\n"))))
            (.append sb "    ;;\n")))))
    (.append sb "esac\n")
    (spit dispatch-path (str sb))
    (.setExecutable (io/file dispatch-path) true)
    dispatch-path))

;; ---------------------------------------------------------------------------
;; Display
;; ---------------------------------------------------------------------------

(def ^:private loop-keys
  #{"ctrl-s" "ctrl-r" "ctrl-b" "ctrl-t" "ctrl-x" "ctrl-u" "ctrl-/"})

(defn- filter-visible
  "Apply active type/source/topic filters to reports."
  [reports {:keys [types sources topics]}
   all-types all-sources all-topics]
  (let [active-types   (or types (set all-types))
        active-sources (or sources (set all-sources))
        active-topics  (or topics (set all-topics))]
    (->> reports
         (filter #(contains? active-types (display-type (:type %))))
         (filter #(or (empty? all-sources)
                      (contains? active-sources (:source %))))
         (filter #(or (empty? all-topics)
                      (and (nil? topics) (nil? (:topic %)))
                      (contains? active-topics (:topic %)))))))

(defn- status-header
  "Build the fzf --header string showing current sort and active filters."
  [sort-idx {:keys [types sources topics]}
   all-types all-sources all-topics]
  (let [active-types   (or types (set all-types))
        active-sources (or sources (set all-sources))
        active-topics  (or topics (set all-topics))]
    (str "[" (first (nth sort-options sort-idx)) "]"
         (when-not (= active-types (set all-types))
           (str " types:" (str/join "," (sort active-types))))
         (when-not (or (empty? all-sources) (= active-sources (set all-sources)))
           (str " src:" (str/join "," (sort active-sources))))
         (when-not (or (empty? all-topics) (= active-topics (set all-topics)))
           (str " topic:" (str/join "," (sort active-topics)))))))

(defn- column-headers
  "Return tab-joined header string for report columns."
  [show-type? show-src? skip]
  (str/join "\t"
            (concat
             (when (and show-type? (not (skip "type")))   ["Type"])
             (when (and show-src?  (not (skip "source"))) ["Source"])
             (when-not (skip "priority") ["P"])
             (when-not (skip "deadline") ["D"])
             (when-not (skip "flags")    ["Flags"])
             (when-not (skip "replies")  ["#"])
             (when-not (skip "author")   ["Author"])
             (when-not (skip "owner")    ["Owner"])
             (when-not (skip "date")     ["Date"])
             (when-not (skip "att")      ["Att"])
             ["Subject"])))

(defn- show-related!
  "Show related reports for the selected report in a nested fzf.
  Resolves related message-ids against the mid-index to display full report rows.
  Supports the same keybindings as the main fzf view."
  [selected-report mid-index config show-type? show-src? skip]
  (when-let [rels (seq (:related selected-report))]
    (let [resolved (keep #(get mid-index (:message-id %)) rels)]
      (when (seq resolved)
        (let [resolved (vec resolved)
          skip   (normalize-skip-columns skip)
          header (column-headers show-type? show-src? skip)
          rows     (mapv #(report->row % show-type? show-src? skip) resolved)
          aligned  (tabulate (cons header rows))
          input    (str/join "\n" aligned)
          dispatch-path (str (System/getProperty "java.io.tmpdir")
                             "/bone-related-" (System/currentTimeMillis) ".sh")]
      (try
        (write-dispatch-script! dispatch-path config resolved)
        (process/shell {:in input :out :string :continue true}
                       "fzf" "--header-lines" "1"
                       "--header" (str "~ " (count resolved) " related to: "
                                      (truncate (:subject selected-report "(no subject)") 60)
                                      " [Ctrl-x: back]")
                       "--no-sort" "--reverse" "--no-hscroll"
                       "--prompt" "related~ "
                       "--bind" (str "enter:execute(" dispatch-path " open {n})")
                       "--bind" (str "ctrl-o:execute-silent(" dispatch-path " browse {n})")
                       "--bind" (str "ctrl-v:execute(" dispatch-path " view {n})")
                       "--bind" (str "ctrl-h:execute(" dispatch-path " help)")
                       "--bind" "ctrl-x:abort"
                       "--bind" "esc:abort")
        true
        (finally
          (.delete (io/file dispatch-path)))))))))


(defn- handle-fzf-key
  "Handle an fzf --expect key. Returns updated state."
  [key-used state all-types all-sources all-topics reload-fn
   & {:keys [selected-report sel-idx mid-index config show-type? show-src? skip-columns]}]
  (let [{:keys [reports sort-idx]} state
        state (dissoc state :cursor-pos)]
    (case key-used
      "ctrl-s" (if-let [idx (pick-sort!)]
                 (assoc state :reports (sort-reports reports idx) :sort-idx idx)
                 state)
      "ctrl-r" (if-let [new-types (pick-multi! "types> " all-types)]
                 (assoc state :types new-types)
                 state)
      "ctrl-b" (if (empty? all-sources)
                 (do (println "  No source information available.") state)
                 (if-let [new-sources (pick-multi! "sources> " all-sources)]
                   (assoc state :sources new-sources)
                   state))
      "ctrl-t" (if (empty? all-topics)
                 (do (println "  No topic information available.") state)
                 (if-let [new-topics (pick-multi! "topics> " all-topics)]
                   (assoc state :topics new-topics)
                   state))
      "ctrl-x" (assoc state :types nil :sources nil :topics nil)
      "ctrl-/" (do (if (and selected-report (seq (:related selected-report)))
                     (or (show-related! selected-report mid-index config
                                        show-type? show-src? skip-columns)
                         (println "  No related reports in loaded data."))
                     (println "  No related reports."))
                   (cond-> state sel-idx (assoc :cursor-pos sel-idx)))
      "ctrl-u" (if reload-fn
                 (do (println "  Updating cache...")
                     (update-sources-cache!)
                     (let [new-reports (reload-fn)]
                       (assoc state
                              :reports (sort-reports new-reports sort-idx)
                              :types nil :sources nil :topics nil)))
                 (do (println "  Cache update not available for this data source.")
                     state))
      ;; enter/ctrl-o/ctrl-v handled by execute, or esc
      state)))

(defn display-reports!
  "Display reports interactively with fzf, or as plain text lines.
  reload-fn, when non-nil, is called on ctrl-u to refresh the cache and
  return a new {:reports ...} map."
  [config reports & {:keys [reload-fn skip-columns]}]
  (let [show-type?   true
        show-src?    (multiple-sources? reports)
        skip-columns (normalize-skip-columns (or skip-columns (:skip-columns config)))
        dispatch-path (str (System/getProperty "java.io.tmpdir")
                           "/bone-dispatch-" (System/currentTimeMillis) ".sh")]
    (if (empty? reports)
      (println "No reports found.")
      (if (fzf-available?)
        (try
          (loop [{:keys [reports sort-idx] :as state}
                 {:reports reports :sort-idx 0 :types nil :sources nil :topics nil}]
            (let [mid-index (into {} (keep (fn [r]
                                             (when-let [mid (:message-id r)]
                                               [mid r])))
                                     reports)
                  all-types   (vec (distinct (map (comp display-type :type) reports)))
                    all-sources (vec (distinct (keep :source reports)))
                    all-topics  (vec (distinct (keep :topic reports)))
                    visible     (filter-visible reports state all-types all-sources all-topics)
                    header      (column-headers show-type? show-src? skip-columns)
                    rows        (mapv #(report->row % show-type? show-src? skip-columns) visible)
                    aligned     (tabulate (cons header rows))
                    input       (str/join "\n" aligned)
                    _           (write-dispatch-script! dispatch-path config visible)
                    cursor-pos  (:cursor-pos state)
                    fzf-args    (cond-> ["fzf" "--header-lines" "1"
                                         "--header" (status-header sort-idx state
                                                                   all-types all-sources all-topics)
                                         "--no-sort" "--reverse" "--no-hscroll"
                                         "--prompt" "report> "
                                         "--expect" (str/join "," loop-keys)
                                         "--bind" (str "enter:execute(" dispatch-path " open {n})")
                                         "--bind" (str "ctrl-o:execute-silent(" dispatch-path " browse {n})")
                                         "--bind" (str "ctrl-v:execute(" dispatch-path " view {n})")
                                         "--bind" (str "ctrl-h:execute(" dispatch-path " help)")
                                         "--bind" "ctrl-n:down"
                                         "--bind" "ctrl-p:up"]
                                  cursor-pos (conj "--bind" (str "load:pos(" (inc cursor-pos) ")")))
                    {:keys [exit out]}
                    (apply process/shell {:in input :out :string :continue true} fzf-args)]
                (let [lines      (when (seq (str/trim out))
                                   (str/split-lines (str/trim out)))
                      key-used   (first lines)
                      selected   (second lines)
                      sel-idx    (when selected
                                   (some (fn [i] (when (= (nth aligned (inc i)) selected) i))
                                         (range (count visible))))
                      sel-report (when sel-idx (nth visible sel-idx))]
                  (when (or (zero? exit) (loop-keys key-used))
                    (recur (handle-fzf-key key-used state
                                           all-types all-sources all-topics
                                           reload-fn
                                           :selected-report sel-report
                                           :sel-idx sel-idx
                                           :mid-index mid-index
                                           :config config
                                           :show-type? show-type?
                                           :show-src? show-src?
                                           :skip-columns skip-columns))))))
          (finally
            (.delete (io/file dispatch-path))))
        ;; Plain text fallback
        (do (println (count reports) "report(s):\n")
            (doseq [r reports]
              (println " " (report->line r show-type? show-src? skip-columns))))))))

;; ---------------------------------------------------------------------------
;; Report (triage summary)
;; ---------------------------------------------------------------------------

(def ^:private default-report-config
  {:sections    ["overview" "stale-patches" "stale-bugs" "active-threads" "expiring" "recent" "owned"]
   :stale-days  14
   :recent-days 7
   :expiry-days 7
   :top-n       10})

(defn- report-age-days
  "Days since report was posted. Returns nil on parse failure."
  [report]
  (let [ms (parse-date-ms (:date-raw report))]
    (when (pos? ms)
      (long (/ (- (System/currentTimeMillis) ms) 86400000)))))

(defn- section-header [title]
  (let [rule (apply str (repeat (max 0 (- 50 (count title) 3)) "─"))]
    (str "── " title " " rule)))

(defn- report-one-liner
  "Format a report as a concise one-line string for report output."
  [report & {:keys [prefix]}]
  (str (when prefix (format "%s  " prefix))
       (truncate (:subject report "(no subject)") 60)
       "  " (:from-name report (:from report "?"))))

(defn- section-overview [all-reports _rcfg]
  (let [open    (remove :closed all-reports)
        closed  (filter :closed all-reports)
        by-type (frequencies (map :type open))
        by-reason (frequencies (map #(:close-reason % "resolved") closed))
        flags   (map #(:flags (report-flags+score %)) open)
        acked   (count (filter #(= (first %) \A) flags))
        owned   (count (filter #(= (second %) \O) flags))
        neither (count (filter #(= % "---") flags))
        open-parts   (str/join ", " (for [[t n] (sort-by val > by-type)] (str (display-type t) ": " n)))
        closed-parts (str/join ", " (for [[r n] (sort-by val > by-reason)] (str r ": " n)))]
    (str (section-header "Overview") "\n"
         "  Open: " (count open) " | " open-parts "\n"
         "Closed: " (count closed) " | " closed-parts "\n"
         "Status: not acked: " neither ", acked: " acked ", owned: " owned "\n")))

(defn- section-stale [type-filter reports rcfg]
  (let [days   (:stale-days rcfg 14)
        top-n  (:top-n rcfg 10)
        label  (str "Stale " type-filter " (>" days " days, not acked, up to " top-n ")")
        stale  (->> reports
                    (filter #(= (:type %) type-filter))
                    (remove :acked)
                    (filter #(when-let [d (report-age-days %)] (> d days)))
                    (sort-by report-age-days >)
                    (take top-n))]
    (when (seq stale)
      (str (section-header label) "\n"
           (str/join "\n" (map #(report-one-liner % :prefix (format "%3dd" (report-age-days %))) stale))
           "\n"))))

(defn- section-active-threads [reports rcfg]
  (let [top-n  (:top-n rcfg 10)
        active (->> reports
                    (filter #(> (:replies % 0) 0))
                    (sort-by :replies >)
                    (take top-n))]
    (when (seq active)
      (str (section-header (str "Active threads (most replies, up to " top-n ")")) "\n"
           (str/join "\n" (map #(report-one-liner % :prefix (format "%3d replies" (:replies % 0))) active))
           "\n"))))

(defn- section-recent [reports rcfg]
  (let [days     (:recent-days rcfg 7)
        cutoff   (- (System/currentTimeMillis) (* days 86400000))
        recent   (->> reports
                      (filter #(> (parse-date-ms (:date-raw %)) cutoff))
                      (sort-by #(parse-date-ms (:date-raw %)) >))]
    (when (seq recent)
      (str (section-header (str "Recent (last " days " days)")) "\n"
           (str/join "\n" (map #(report-one-liner % :prefix (date-only (:date %))) recent))
           "\n"))))

(defn- section-expiring [reports rcfg]
  (let [days  (:expiry-days rcfg 7)
        top-n (:top-n rcfg 10)
        label (str "Expiring soon (within " days " days, up to " top-n ")")
        expiring (->> reports
                      (keep (fn [r]
                              (when-let [d (days-until r :expiry)]
                                (when (<= 0 d days)
                                  (assoc r ::exp-days d)))))
                      (sort-by ::exp-days)
                      (take top-n))]
    (when (seq expiring)
      (str (section-header label) "\n"
           (str/join "\n" (map #(report-one-liner % :prefix (format "%3dd left" (::exp-days %))) expiring))
           "\n"))))

(defn- section-owned [reports _rcfg addresses]
  (when (seq addresses)
    (let [addrs (if (set? addresses) addresses (normalize-addresses addresses))
          owned (->> reports
                     (remove :closed)
                     (filter #(when-let [o (:owned %)]
                                (contains? addrs (str/lower-case o))))
                     (sort-by #(parse-date-ms (:date-raw %)) >))]
      (when (seq owned)
        (str (section-header "Owned (your open reports)") "\n"
             (str/join "\n" (map #(report-one-liner % :prefix (date-only (:date %))) owned))
           "\n")))))

(def ^:private report-section-fns
  {"overview"       (fn [reports rcfg _addrs] (section-overview reports rcfg))
   "stale-patches"  (fn [reports rcfg _addrs] (section-stale "patch" reports rcfg))
   "stale-bugs"     (fn [reports rcfg _addrs] (section-stale "bug" reports rcfg))
   "active-threads" (fn [reports rcfg _addrs] (section-active-threads reports rcfg))
   "expiring"       (fn [reports rcfg _addrs] (section-expiring reports rcfg))
   "recent"         (fn [reports rcfg _addrs] (section-recent reports rcfg))
   "owned"          (fn [reports rcfg addrs]  (section-owned reports rcfg addrs))})

(defn- generate-report
  "Produce a triage report from loaded reports."
  [config reports]
  (let [rcfg      (merge default-report-config (:report config))
        addresses (normalize-addresses (:my-addresses config))
        sections  (:sections rcfg)
        open     (remove :closed reports)]
    (println)
    (doseq [s sections]
      (when-let [f (report-section-fns s)]
        (when-let [out (f (if (= s "overview") reports open) rcfg addresses)]
          (println out))))))

;; ---------------------------------------------------------------------------
;; CLI
;; ---------------------------------------------------------------------------

(defn- usage [] (println usage-text))

(defn- next-arg
  "Return the argument following the first occurrence of a flag in args."
  [args flags]
  (let [av  (vec args)
        idx (first (keep-indexed #(when (flags %2) (inc %1)) av))]
    (when idx (nth av idx nil))))

(defn- parse-opts
  "Parse CLI options from an argument sequence. Returns [opts remaining-args]."
  [args]
  (loop [opts {} [a & more :as remaining] args]
    (cond
      (nil? a)                        [opts nil]
      (= a "-")                       [(assoc opts :data-src :stdin) nil]
      (#{"-f" "--file"} a)            (recur (assoc opts :data-src :file :path (first more)) (rest more))
      (#{"-u" "--url"} a)             (recur (assoc opts :data-src :url  :url  (first more)) (rest more))
      (#{"-U" "--urls-file"} a)       (recur (assoc opts :data-src :urls-file :urls-path (first more)) (rest more))
      (#{"-M" "--my-addresses"} a)    (recur (assoc opts :my-addresses (str/split (first more) #",")) (rest more))
      (#{"-n" "--source"} a)          (recur (assoc opts :source-filter (first more)) (rest more))
      (#{"-p" "--min-priority"} a)    (recur (assoc opts :min-priority (some-> (first more) parse-long)) (rest more))
      (#{"-s" "--min-score"} a)       (recur (assoc opts :min-score (some-> (first more) parse-long)) (rest more))
      (#{"-m" "--mine"} a)            (recur (assoc opts :mine? true) more)
      (#{"-c" "--closed"} a)          (recur (assoc opts :closed? true) more)
      (#{"-S" "--skip-columns"} a)    (recur (assoc opts :skip-columns (str/split (first more) #",")) (rest more))
      :else                           [opts remaining])))

(defn- validate-opts!
  "Validate parsed options. Throws on missing flag arguments or invalid values."
  [opts]
  (when (and (= (:data-src opts) :file) (nil? (:path opts)))
    (throw (ex-info "Missing argument for --file" {})))
  (when (and (= (:data-src opts) :url) (nil? (:url opts)))
    (throw (ex-info "Missing argument for --url" {})))
  (when (and (= (:data-src opts) :urls-file) (nil? (:urls-path opts)))
    (throw (ex-info "Missing argument for --urls-file" {})))
  (when (and (contains? opts :my-addresses) (nil? (:my-addresses opts)))
    (throw (ex-info "Missing argument for --my-addresses" {})))
  (when (and (contains? opts :source-filter) (nil? (:source-filter opts)))
    (throw (ex-info "Missing argument for --source" {})))
  (when (and (contains? opts :min-priority) (nil? (:min-priority opts)))
    (throw (ex-info "Missing or invalid argument for --min-priority" {})))
  (when (and (:min-priority opts) (not (#{1 2 3} (:min-priority opts))))
    (throw (ex-info (str "Invalid --min-priority: " (:min-priority opts) " (must be 1, 2, or 3)") {})))
  (when (and (contains? opts :min-score) (nil? (:min-score opts)))
    (throw (ex-info "Missing or invalid argument for --min-score" {})))
  (when (and (:min-score opts) (not (<= 0 (:min-score opts) 7)))
    (throw (ex-info (str "Invalid --min-score: " (:min-score opts) " (must be 0–7)") {})))
  opts)

(defn- load-data
  "Load reports from the source specified in opts."
  [opts]
  (case (:data-src opts)
    :file      (load-from-file (:path opts))
    :url       (load-from-url (:url opts))
    :urls-file (load-from-urls-file (:urls-path opts))
    :stdin     (load-from-stdin)
    (load-from-sources)))

(defn- filter-reports
  "Apply CLI filters to a reports vector."
  [reports {:keys [source-filter mine? my-addresses min-priority min-score closed?]}]
  (cond->> reports
    source-filter              (filter #(= (:source %) source-filter))
    (and mine? my-addresses)   (filter #(involves-email? % my-addresses))
    min-priority         (filter #(>= (:priority % 0) min-priority))
    min-score            (filter #(>= (:score (report-flags+score %)) min-score))
    (not closed?)        (remove :closed)))

(defn- prepare-opts
  "Parse, validate, and enrich CLI options with email from config."
  [args config]
  (let [[opts _] (parse-opts args)
        opts     (validate-opts! opts)
        addrs    (or (:my-addresses opts) (:my-addresses config))
        addrs    (when addrs (if (string? addrs) [addrs] addrs))
        opts     (assoc opts :my-addresses addrs)]
    (when (and (:mine? opts) (not (seq addrs)))
      (throw (ex-info "No address configured. Set :my-addresses in ~/.config/bone/config.edn or use -M EMAIL[,EMAIL,...]." {})))
    opts))

(defn -main [& args]
  (let [args   (seq (or (seq args) *command-line-args*))
        config (load-config)]
    (cond
      (some #{"-h" "--help"} args)
      (usage)

      (some #{"clear"} args)
      (clear-cache!)

      (some #{"update"} args)
      (update-sources-cache!)

      (some #{"report"} args)
      (let [opts    (prepare-opts (remove #{"report"} args) config)
            reports (filter-reports (:reports (load-data opts)) opts)
            cfg     (cond-> config (:my-addresses opts) (assoc :my-addresses (:my-addresses opts)))]
        (generate-report cfg reports))

      (some #{"-l" "--list-sources"} args)
      (list-sources!)

      (some #{"-a" "--add-source"} args)
      (if-let [src (next-arg args #{"-a" "--add-source"})]
        (add-source! src)
        (println "Usage: bone -a URL_OR_PATH"))

      (some #{"-r" "--remove-source"} args)
      (if-let [src (next-arg args #{"-r" "--remove-source"})]
        (remove-source! src)
        (println "Usage: bone -r URL_OR_PATH"))

      :else
      (let [opts      (prepare-opts args config)
            reports   (filter-reports (:reports (load-data opts)) opts)
            reload-fn (when (nil? (:data-src opts))
                        #(filter-reports (:reports (load-from-sources)) opts))]
        (display-reports! config reports
                         :reload-fn reload-fn
                         :skip-columns (:skip-columns opts))))))

(when (= *file* (System/getProperty "babashka.file"))
  (try
    (apply -main *command-line-args*)
    (catch clojure.lang.ExceptionInfo e
      (binding [*out* *err*]
        (println (.getMessage e)))
      (System/exit 1))))
