#!/usr/bin/env bb

;; bone.clj — Browse BARK reports interactively.
;;
;; Standalone CLI tool: reads JSON produced by bark export from a file,
;; a URL, stdin, or a URLs file listing multiple report sources.
;; Displays via fzf (with detail on selection) or plain text fallback.
;;
;; Configuration (~/.config/bone/config.edn):
;;   {:email "you@example.com"}
;;
;; Usage:
;;   bone.clj [options]
;;   bone.clj clear           Empty the cache
;;   bone.clj update          Fetch/update reports from all sources
;;
;; Options:
;;   -f, --file FILE          Read reports from a JSON file
;;   -u, --url  URL           Fetch reports from a URL
;;   -U, --urls-file FILE     Fetch & merge reports from URLs listed in FILE
;;   -e, --email EMAIL        Your email (overrides config)
;;   -p, --min-priority 1-3   Only show reports with priority >= N
;;   -s, --min-status 1-7     Only show reports with status >= N
;;   -n, --source NAME        Filter by source name
;;   -m, --mine               Show only reports involving your email
;;   -c, --closed             Include closed reports
;;   -a, --add-source PATH    Add a reports.json source
;;   -                        Read JSON from stdin
;;
;; By default, bone shows all open reports from configured sources.

(ns bzg.bone
  (:require [babashka.process :as process]
            [babashka.http-client :as http]
            [cheshire.core :as json]
            [clojure.string :as str]
            [clojure.pprint :as pprint]
            [clojure.java.io :as io]))

;; ---------------------------------------------------------------------------
;; Config
;; ---------------------------------------------------------------------------

(def config-path
  (str (System/getProperty "user.home") "/.config/bone/config.edn"))

(def sources-path
  (str (System/getProperty "user.home") "/.config/bone/sources.json"))

(def cache-base
  (str (System/getProperty "user.home") "/.config/bone/cache"))

(def reports-cache-dir
  (str (System/getProperty "user.home") "/.config/bone/reports-cache"))

(defn- load-config []
  (let [f (io/file config-path)]
    (if (.exists f)
      (read-string (slurp f))
      {})))

(defn- load-sources
  "Load sources.json — a JSON array of URL or file path strings."
  []
  (let [f (io/file sources-path)]
    (if (.exists f)
      (json/parse-string (slurp f))
      [])))

(defn- save-sources! [sources]
  (.mkdirs (.getParentFile (io/file sources-path)))
  (spit sources-path (json/generate-string (vec (distinct sources)))))

(defn- add-source! [src]
  (let [sources (load-sources)]
    (if (some #{src} sources)
      (println (str "Already registered: " src))
      (do (save-sources! (conj sources src))
          (println (str "Added: " src))))))

(defn- remove-source! [src]
  (let [sources (load-sources)]
    (if (some #{src} sources)
      (do (save-sources! (remove #{src} sources))
          (println (str "Removed: " src)))
      (println (str "Not found: " src)))))

(defn- list-sources! []
  (let [sources (load-sources)]
    (if (empty? sources)
      (println "No sources configured. Use --add-source URL_OR_PATH to add one.")
      (doseq [s sources]
        (println (str "  " s))))))

;; ---------------------------------------------------------------------------
;; Data loading
;; ---------------------------------------------------------------------------

(defn- load-json-string [s]
  (json/parse-string s keyword))

(defn- unwrap-envelope
  "Unwrap a reports.json envelope. Returns {:reports [...] :meta {...}}.
  Injects the envelope's :source into each report that lacks one.
  Handles: new per-source format {source, reports, list-id, ...}
  and legacy bare-array format."
  [data]
  (if (sequential? data)
    {:reports data :meta {}}
    (let [reports    (or (:reports data) [])
          meta       (dissoc data :reports)
          src-name   (:source meta)
          reports    (if src-name
                       (mapv #(if (:source %) % (assoc % :source src-name)) reports)
                       reports)]
      {:reports reports :meta meta})))

(defn- load-from-file [path]
  (when-not (.exists (io/file path))
    (binding [*out* *err*]
      (println (str "File not found: " path)))
    (System/exit 1))
  (unwrap-envelope (load-json-string (slurp path))))

(defn- load-from-url [url]
  (let [resp (http/get url {:headers {"Accept" "application/json"}})]
    (when (not= 200 (:status resp))
      (binding [*out* *err*]
        (println (str "HTTP " (:status resp) " fetching " url)))
      (System/exit 1))
    (unwrap-envelope (load-json-string (:body resp)))))

(defn- load-from-stdin []
  (unwrap-envelope (load-json-string (slurp *in*))))

(defn- load-from-urls-file [path]
  (when-not (.exists (io/file path))
    (binding [*out* *err*]
      (println (str "URLs file not found: " path)))
    (System/exit 1))
  (let [urls (->> (str/split-lines (slurp path))
                  (map str/trim)
                  (remove #(or (str/blank? %) (str/starts-with? % "#"))))]
    (reduce (fn [acc u]
              (let [{:keys [reports meta]} (load-from-url u)]
                (-> acc
                    (update :reports into reports)
                    (update :meta merge meta))))
            {:reports [] :meta {}}
            urls)))

(defn- url? [s]
  (boolean (re-find #"^https?://" s)))

(defn- source-base-dir
  "Compute the base directory from a source path/URL.
  For file paths, returns the parent directory.
  For file:// URIs, strips the scheme and returns parent.
  For HTTP URLs, returns nil (use bark-path instead)."
  [src]
  (let [path (cond
               (str/starts-with? src "file://") (str/replace-first src "file://" "")
               (url? src)                       nil
               :else                            src)]
    (when path
      (let [parent (.getParent (io/file path))]
        (when parent (str parent "/"))))))

(defn- load-one-source
  "Load reports from a single source string (URL, file:// URI, or file path).
  Returns {:reports [...] :meta {...}} with :base-dir injected into meta."
  [src]
  (let [result (cond
                 (url? src)                        (load-from-url src)
                 (str/starts-with? src "file://")  (load-from-file (str/replace-first src "file://" ""))
                 :else                             (load-from-file src))
        base   (source-base-dir src)]
    (if base
      (assoc-in result [:meta :base-dir] base)
      result)))

(defn- source->cache-file
  "Deterministic cache filename for a source URL/path."
  [src]
  (let [safe (-> src
                 (str/replace #"[^a-zA-Z0-9._-]" "_")
                 (subs 0 (min 200 (count src))))]
    (str reports-cache-dir "/" safe ".json")))

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
  (doseq [dir [(io/file cache-base) (io/file reports-cache-dir)]]
    (when (.exists dir)
      (run! #(.delete %) (reverse (file-seq dir)))))
  (println "Cache cleared."))

(defn- load-one-source-cached
  "Load reports from cache if available, otherwise fetch and cache."
  [src]
  (if-let [cached (cache-read src)]
    (let [result (unwrap-envelope cached)
          base   (source-base-dir src)]
      (if base (assoc-in result [:meta :base-dir] base) result))
    (let [resp   (when (url? src)
                   (http/get src {:headers {"Accept" "application/json"}}))
          body   (if (url? src)
                   (do (when (not= 200 (:status resp))
                         (binding [*out* *err*]
                           (println (str "HTTP " (:status resp) " fetching " src)))
                         (System/exit 1))
                       (:body resp))
                   (slurp (let [p (if (str/starts-with? src "file://")
                                    (str/replace-first src "file://" "")
                                    src)]
                            p)))]
      (cache-write! src body)
      (let [result (unwrap-envelope (load-json-string body))
            base   (source-base-dir src)]
        (if base (assoc-in result [:meta :base-dir] base) result)))))

(defn- update-sources-cache!
  "Fetch all configured sources and update cache."
  []
  (let [sources (load-sources)]
    (when (empty? sources)
      (println "No sources configured.")
      (System/exit 1))
    (doseq [src sources]
      (try
        (let [body (if (url? src)
                     (let [resp (http/get src {:headers {"Accept" "application/json"}})]
                       (when (not= 200 (:status resp))
                         (throw (ex-info "HTTP error" {:status (:status resp)})))
                       (:body resp))
                     (slurp (if (str/starts-with? src "file://")
                              (str/replace-first src "file://" "")
                              src)))]
          (cache-write! src body)
          (println (str "  Updated: " src)))
        (catch Exception e
          (binding [*out* *err*]
            (println (str "  [warn] Failed to update " src ": " (.getMessage e)))))))))

(defn- load-from-sources
  "Load and merge reports from all configured sources."
  []
  (let [sources (load-sources)]
    (when (empty? sources)
      (binding [*out* *err*]
        (println "No sources configured and no -f/-u/-U/- given.")
        (println "Add a source:  bone --add-source URL_OR_PATH")
        (println "Or use:        bone -f FILE | -u URL | -"))
      (System/exit 1))
    (reduce (fn [acc src]
              (try
                (let [{:keys [reports meta]} (load-one-source-cached src)]
                  (-> acc
                      (update :reports into reports)
                      (update :meta merge meta)))
                (catch Exception e
                  (binding [*out* *err*]
                    (println (str "  [warn] Failed to load " src ": " (.getMessage e))))
                  acc)))
            {:reports [] :meta {}}
            sources)))

;; ---------------------------------------------------------------------------
;; Filtering
;; ---------------------------------------------------------------------------

(defn- involves-email?
  "True when the report involves the given email address."
  [report email]
  (let [e (str/lower-case email)]
    (some #(when % (= (str/lower-case %) e))
          [(:from report) (:acked-by report) (:owned-by report) (:closed-by report)])))

(defn- filter-mine [reports email]
  (filter #(involves-email? % email) reports))

(defn- filter-by-source [reports src-name]
  (filter #(= (:source %) src-name) reports))

(defn- filter-by-priority [reports min-p]
  (filter #(>= (:priority % 0) min-p) reports))

(defn- filter-by-status [reports min-s]
  (filter #(>= (:status % 0) min-s) reports))

(defn- filter-open [reports]
  (remove :closed reports))

;; ---------------------------------------------------------------------------
;; Formatting helpers
;; ---------------------------------------------------------------------------

(defn- has-source? [reports]
  (some :source reports))

(defn- report->row
  "Format a report as a tab-separated row for fzf display."
  [report show-type? show-src?]
  (str/join "\t"
            (concat
             (when show-type? [(:type report "")])
             (when show-src?   [(:source report "")])
             [(str (:priority report 0))
              (:flags report "---")
              (str (:replies report 0))
              (:from report "?")
              (:date report "")
              (:subject report "(no subject)")])))

(defn- extra-str [report]
  (let [parts (remove nil?
                      [(:source report)
                       (:version report)
                       (:topic report)
                       (:patch-seq report)
                       (when-let [sources (seq (:patch-source report))]
                         (str "src:" (str/join "," sources)))
                       (when-let [ps (seq (:patches report))]
                         (str "📎" (count ps)))
                       (when-let [v (:votes report)] (str "votes:" v))
                       (when-let [s (:series report)]
                         (str "series:" (:received s) "/" (:expected s)
                              (when (:closed s) " closed")))
                       (when-let [related (seq (:related report))]
                         (str "→" (str/join "," (distinct (map :type related)))))])]
    (when (seq parts) (str/join " " parts))))

(defn- report->line
  "Format a report as a plain text line."
  [report show-type? show-src?]
  (str (when show-type? (format "[%-12s] " (:type report "")))
       (when show-src?   (format "[%-10s] " (:source report "")))
       (format "%d %-3s %3d %-25s %s  %s"
               (:priority report 0)
               (:flags report "---")
               (:replies report 0)
               (:from report "?")
               (:date report "")
               (:subject report "(no subject)"))
       (when-let [e (extra-str report)] (str " " e))))

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

(defn- browse-cmd
  "Return the best available command for viewing a URL.
  Prefers terminal browsers, falls back to platform opener."
  []
  (cond
    (command-available? "w3m")   ["w3m" "-o" "confirm_qq=false"]
    (command-available? "lynx")  ["lynx"]
    (command-available? "links") ["links"]
    :else (let [os (str/lower-case (System/getProperty "os.name"))]
            (cond
              (str/includes? os "mac") ["open"]
              (str/includes? os "win") ["start"]
              :else                    ["xdg-open"]))))

(defn- patch-paths
  "Return a vector of {:url ... :cache-path ...} for each patch in a report.
  Resolves URLs from base-dir or bark-path, and cache paths from source name."
  [report meta]
  (when-let [ps (seq (:patches report))]
    (let [base       (or (:base-dir meta)
                         (when-let [bp (:bark-path meta)]
                           (if (str/ends-with? bp "/") bp (str bp "/"))))
          src-name   (or (:source meta) "default")]
      (mapv (fn [p]
              (let [file (:file p)]
                {:url        (if base (str base file) file)
                 :cache-path (str cache-base "/" src-name "/" file)
                 :filename   (:patch/filename p (:file p))}))
            ps))))

(defn- fetch-to-cache!
  "Ensure a patch file is in the cache. Returns the cache path.
  For local files, copies. For URLs, downloads."
  [url cache-path]
  (let [f (io/file cache-path)]
    (when-not (.exists f)
      (.mkdirs (.getParentFile f))
      (if (re-find #"^https?://" url)
        (let [resp (http/get url)]
          (if (= 200 (:status resp))
            (spit cache-path (:body resp))
            (println (str "  Failed to fetch: " url " (HTTP " (:status resp) ")"))))
        ;; Local file — copy
        (let [src (io/file url)]
          (if (.exists src)
            (io/copy src f)
            (println (str "  File not found: " url))))))
    cache-path))

(defn- pager-cmd
  "Return the pager command: $PAGER, or 'less', or 'more'."
  []
  (or (System/getenv "PAGER") "less"))

(defn- show-patch!
  "Fetch a patch to cache and display it with a pager."
  [report meta]
  (if-let [paths (seq (patch-paths report meta))]
    (if (= 1 (count paths))
      ;; Single patch — fetch and page
      (let [{:keys [url cache-path]} (first paths)
            cached (fetch-to-cache! url cache-path)]
        (when (.exists (io/file cached))
          (process/shell {:continue true} (pager-cmd) cached)))
      ;; Multiple patches — let user pick with fzf, then page
      (let [labels (mapv (fn [{:keys [url cache-path]}]
                           (last (str/split url #"/")))
                         paths)
            input  (str/join "\n" labels)
            {:keys [exit out]}
            (process/shell {:in input :out :string :continue true}
                           "fzf" "--prompt" "patch> " "--no-sort" "--reverse")]
        (when (zero? exit)
          (let [selected (str/trim out)
                idx      (.indexOf ^java.util.List labels selected)]
            (when (>= idx 0)
              (let [{:keys [url cache-path]} (nth paths idx)
                    cached (fetch-to-cache! url cache-path)]
                (when (.exists (io/file cached))
                  (process/shell {:continue true} (pager-cmd) cached))))))))
    (println "  No patches for this report.")))

(defn- open-url! [url]
  (try
    (let [cmd (browse-cmd)]
      (apply process/shell {:continue true} (conj cmd url)))
    (catch Exception _
      (println (str "  Failed to open: " url)))))

;; ---------------------------------------------------------------------------
;; Sorting
;; ---------------------------------------------------------------------------

(defn- parse-date-ms
  "Parse a date-raw string to epoch millis for sorting. Returns 0 on failure."
  [s]
  (when (and s (not (str/blank? s)))
    (try
      ;; Java Date.toString() format: "Sat Mar 07 04:10:11 CET 2026"
      (let [fmt (java.text.SimpleDateFormat. "EEE MMM dd HH:mm:ss z yyyy"
                                              java.util.Locale/ENGLISH)]
        (.getTime (.parse fmt s)))
      (catch Exception _
        ;; Try ISO-ish format: "2026-03-07..."
        (try
          (.toEpochMilli (java.time.Instant/parse s))
          (catch Exception _ 0))))))

(def sort-options
  [["date (newest)"    (fn [r] (- (or (parse-date-ms (:date-raw r)) 0)))  compare]
   ["date (oldest)"    (fn [r] (or (parse-date-ms (:date-raw r)) 0))      compare]
   ["priority (high)"  (fn [r] (- (:priority r 0)))                        compare]
   ["replies (most)"   (fn [r] (- (:replies r 0)))                         compare]
   ["type"             (fn [r] (:type r ""))                                compare]
   ["author"           (fn [r] (str/lower-case (:from r "")))              compare]])

(defn- pick-sort!
  "Let user pick a sort order via fzf. Returns index or nil."
  []
  (let [labels (mapv first sort-options)
        input  (str/join "\n" labels)
        {:keys [exit out]}
        (process/shell {:in input :out :string :continue true}
                       "fzf" "--prompt" "sort by> " "--no-sort" "--reverse")]
    (when (zero? exit)
      (let [selected (str/trim out)]
        (.indexOf ^java.util.List labels selected)))))

(defn- sort-reports [reports sort-idx]
  (let [[_ key-fn cmp] (nth sort-options sort-idx)]
    (sort-by key-fn cmp reports)))

(defn- pick-types!
  "Let user pick which report types to show via fzf multi-select.
  '[all]' resets to all types. Returns a set of type strings, or nil on cancel."
  [all-types active-types]
  (let [options (cons "[all]" all-types)
        input   (str/join "\n" options)
        {:keys [exit out]}
        (process/shell {:in input :out :string :continue true}
                       "fzf" "--multi" "--prompt" "types> "
                       "--no-sort" "--reverse")]
    (when (zero? exit)
      (let [selected (set (remove str/blank? (str/split-lines (str/trim out))))]
        (if (contains? selected "[all]")
          (set all-types)
          selected)))))

(defn- pick-sources!
  "Let user pick which sources to show via fzf multi-select.
  '[all]' resets to all sources. Returns a set of source strings, or nil on cancel."
  [all-sources active-sources]
  (let [options (cons "[all]" all-sources)
        input   (str/join "\n" options)
        {:keys [exit out]}
        (process/shell {:in input :out :string :continue true}
                       "fzf" "--multi" "--prompt" "sources> "
                       "--no-sort" "--reverse")]
    (when (zero? exit)
      (let [selected (set (remove str/blank? (str/split-lines (str/trim out))))]
        (if (contains? selected "[all]")
          (set all-sources)
          selected)))))

;; ---------------------------------------------------------------------------
;; Display
;; ---------------------------------------------------------------------------

(defn display-reports!
  "Display reports interactively with fzf, or as plain text lines."
  [reports meta]
  (let [show-type? true
        show-src?   (has-source? reports)
        all-types   (vec (distinct (map :type reports)))
        all-sources (vec (distinct (keep :source reports)))]
    (if (empty? reports)
      (println "No reports found.")
      (if (fzf-available?)
        (loop [sorted-reports reports
               sort-idx       0
               active-types   (set all-types)
               active-sources (set all-sources)]
          (let [visible  (->> sorted-reports
                              (filter #(contains? active-types (:type %)))
                              (filter #(or (empty? all-sources)
                                           (contains? active-sources (:source %)))))
                header   (str/join "\t"
                                   (concat
                                    (when show-type? ["Type"])
                                    (when show-src?   ["Source"])
                                    ["P" "Flags" "#" "From" "Date" "Subject"]))
                rows     (mapv #(report->row % show-type? show-src?) visible)
                aligned      (tabulate (cons header rows))
                header-line  (first aligned)
                aligned-rows (vec (rest aligned))
                input        (str/join "\n" aligned-rows)
                sort-label   (first (nth sort-options sort-idx))
                type-label   (if (= active-types (set all-types))
                               ""
                               (str " types:" (str/join "," (sort active-types))))
                src-label    (if (or (empty? all-sources)
                                    (= active-sources (set all-sources)))
                               ""
                               (str " src:" (str/join "," (sort active-sources))))
                {:keys [exit out]}
                (process/shell {:in input :out :string :continue true}
                               "fzf" "--header" (str header-line "  [" sort-label "]" type-label src-label)
                               "--no-sort" "--reverse" "--no-hscroll"
                               "--prompt" "report> "
                               "--expect" "enter,ctrl-p,ctrl-o,ctrl-s,ctrl-t,ctrl-n")]
            (when (zero? exit)
              (let [lines    (str/split-lines (str/trim out))
                    key-used (first lines)
                    selected (second lines)]
                (case key-used
                  "ctrl-s"
                  (if-let [new-idx (pick-sort!)]
                    (recur (sort-reports reports new-idx) new-idx active-types active-sources)
                    (recur sorted-reports sort-idx active-types active-sources))

                  "ctrl-t"
                  (if-let [new-types (pick-types! all-types active-types)]
                    (recur sorted-reports sort-idx new-types active-sources)
                    (recur sorted-reports sort-idx active-types active-sources))

                  "ctrl-n"
                  (if (empty? all-sources)
                    (do (println "  No source information available.")
                        (recur sorted-reports sort-idx active-types active-sources))
                    (if-let [new-sources (pick-sources! all-sources active-sources)]
                      (recur sorted-reports sort-idx active-types new-sources)
                      (recur sorted-reports sort-idx active-types active-sources)))

                  ;; enter, ctrl-p, ctrl-o
                  (do
                    (when selected
                      (let [idx (.indexOf ^java.util.List aligned-rows selected)]
                        (when (>= idx 0)
                          (let [report (nth visible idx)]
                            (case key-used
                              "ctrl-p"
                              (show-patch! report meta)

                              "ctrl-o"
                              (if-let [url (:archived-at report)]
                                (let [os  (str/lower-case (System/getProperty "os.name"))
                                      cmd (cond
                                            (str/includes? os "mac") "open"
                                            (str/includes? os "win") "start"
                                            :else                    "xdg-open")]
                                  (try
                                    (process/shell {:continue true} cmd url)
                                    (catch Exception _
                                      (println (str "  Failed to open: " url)))))
                                (println "  No archived-at URL for this report."))

                              ;; enter (default)
                              (if-let [url (:archived-at report)]
                                (open-url! url)
                                (do (println "  No archived-at URL for this report.")
                                    (pprint/pprint report))))))))
                    (recur sorted-reports sort-idx active-types active-sources)))))))
        ;; Plain text fallback
        (do (println (str (count reports) " report(s):\n"))
            (doseq [r reports]
              (println (str "  " (report->line r show-type? show-src?)))))))))

;; ---------------------------------------------------------------------------
;; CLI
;; ---------------------------------------------------------------------------

(defn- usage []
  (println "Usage: bone.clj [options]")
  (println)
  (println "Options:")
  (println "  -f, --file FILE            Read reports from a JSON file")
  (println "  -u, --url  URL             Fetch reports from a URL")
  (println "  -U, --urls-file FILE       Fetch & merge from URLs in FILE (one per line)")
  (println "  -e, --email EMAIL          Your email (overrides ~/.config/bone/config.edn)")
  (println "  -n, --source NAME          Filter by source name")
  (println "  -p, --min-priority 1|2|3   Only show reports with priority >= N")
  (println "  -s, --min-status 1-7       Only show reports with status >= N")
  (println "  -m, --mine                 Show only reports involving your email")
  (println "  -c, --closed               Include closed reports")
  (println "  -                          Read JSON from stdin")
  (println)
  (println "Source management:")
  (println "  -a, --add-source URL_OR_PATH   Add a reports.json source")
  (println "  --remove-source URL_OR_PATH    Remove a source")
  (println "  --list-sources                 List configured sources")
  (println)
  (println "Cache management:")
  (println "  clear                          Empty the cache")
  (println "  update                         Fetch/update reports from sources")
  (println)
  (println "Keys (fzf):")
  (println "  Enter                      View report in terminal browser")
  (println "  Ctrl-o                     Open report in system browser")
  (println "  Ctrl-p                     View patch (fetched to cache)")
  (println "  Ctrl-s                     Change sort order")
  (println "  Ctrl-t                     Filter by report type")
  (println "  Ctrl-n                     Filter by source")
  (println)
  (println "With no -f/-u/-U/- option, bone reads cached reports or fetches")
  (println "once from ~/.config/bone/sources.json. Use 'bone update' to refresh.")
  (println)
  (println "Config: ~/.config/bone/config.edn  e.g. {:email \"you@example.com\"}"))

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

      (some #{"--list-sources"} args)
      (list-sources!)

      (some #{"-a" "--add-source"} args)
      (let [av   (vec (or args []))
            idx  (some #(when (#{"-a" "--add-source"} (nth av % nil)) %)
                       (range (count av)))
            src  (when idx (nth av (inc idx) nil))]
        (if src
          (add-source! src)
          (println "Usage: bone -a URL_OR_PATH")))

      (some #{"--remove-source"} args)
      (let [idx (.indexOf ^java.util.List (vec args) "--remove-source")
            src (nth (vec args) (inc idx) nil)]
        (if src
          (remove-source! src)
          (println "Usage: bone --remove-source URL_OR_PATH")))

      :else
      (let [[opts _]
            (loop [opts {} [a & more :as remaining] args]
              (cond
                (nil? a)                        [opts nil]
                (= a "-")                       [(assoc opts :data-src :stdin) nil]
                (#{"-f" "--file"} a)            (recur (assoc opts :data-src :file :path (first more)) (rest more))
                (#{"-u" "--url"} a)             (recur (assoc opts :data-src :url  :url  (first more)) (rest more))
                (#{"-U" "--urls-file"} a)       (recur (assoc opts :data-src :urls-file :urls-path (first more)) (rest more))
                (#{"-e" "--email"} a)           (recur (assoc opts :email (first more)) (rest more))
                (#{"-n" "--source"} a)          (recur (assoc opts :source-filter (first more)) (rest more))
                (#{"-p" "--min-priority"} a)    (recur (assoc opts :min-priority (parse-long (first more))) (rest more))
                (#{"-s" "--min-status"} a)      (recur (assoc opts :min-status (parse-long (first more))) (rest more))
                (#{"-m" "--mine"} a)            (recur (assoc opts :mine? true) more)
                (#{"-c" "--closed"} a)          (recur (assoc opts :closed? true) more)
                :else                           [opts remaining]))
            email        (or (:email opts) (:email config))
            mine?        (:mine? opts)
            closed?      (:closed? opts)
            min-priority (:min-priority opts)
            min-status   (:min-status opts)]
        (when (and mine? (not email))
          (binding [*out* *err*]
            (println "No email configured. Set :email in ~/.config/bone/config.edn or use -e EMAIL."))
          (System/exit 1))
        (when (and min-priority (not (#{1 2 3} min-priority)))
          (binding [*out* *err*] (println (str "Invalid --min-priority: " min-priority " (must be 1, 2, or 3)")))
          (System/exit 1))
        (when (and min-status (not (<= 1 min-status 7)))
          (binding [*out* *err*] (println (str "Invalid --min-status: " min-status " (must be 1–7)")))
          (System/exit 1))
        (let [data-src (:data-src opts)
              raw-result
              (case data-src
                :file      (load-from-file (:path opts))
                :url       (load-from-url (:url opts))
                :urls-file (load-from-urls-file (:urls-path opts))
                :stdin     (load-from-stdin)
                ;; No explicit source → read from sources.json
                (load-from-sources))
              ;; Inject base-dir for -f local files
              {:keys [reports meta]}
              (if (and (= data-src :file) (nil? (:base-dir (:meta raw-result))))
                (let [base (source-base-dir (:path opts))]
                  (if base
                    (assoc-in raw-result [:meta :base-dir] base)
                    raw-result))
                raw-result)
              reports (if-let [src (:source-filter opts)]
                        (filter-by-source reports src) reports)
              reports (if (and mine? email)
                        (filter-mine reports email) reports)
              reports (if min-priority
                        (filter-by-priority reports min-priority) reports)
              reports (if min-status
                        (filter-by-status reports min-status) reports)
              reports (if-not closed?
                        (filter-open reports) reports)]
          (display-reports! reports meta))))))

(when (= *file* (System/getProperty "babashka.file"))
  (apply -main *command-line-args*))
