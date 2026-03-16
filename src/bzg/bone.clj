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
;;   -s, --min-score 0-7     Only show reports with status score >= N
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

(def sources-path
  (str (System/getProperty "user.home") "/.config/bone/sources.json"))

(def cache-dir
  (str (System/getProperty "user.home") "/.config/bone/cache"))

(def patches-cache-dir
  (str cache-dir "/patches"))

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

(def supported-format-version "0.2.2")

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
  For HTTP URLs, returns nil (use bark-path instead)."
  [src]
  (when-not (url? src)
    (when-let [parent (.getParent (io/file (strip-file-scheme src)))]
      (str parent "/"))))

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
  Injects :source and :bark-path from envelope into each report.
  Handles: new per-source format {source, reports, list-id, ...}
  and legacy bare-array format."
  [data]
  (if (sequential? data)
    {:reports data}
    (let [fv (:format-version data)]
      (when (and fv (not= fv supported-format-version))
        (binding [*out* *err*]
          (println (str "Warning: format-version " fv
                        " differs from supported version "
                        supported-format-version))))
      (let [reports   (or (:reports data) [])
            src-name  (:source data)
            bark-path (:bark-path data)]
        {:reports (mapv (fn [r]
                          (cond-> r
                            (and src-name (not (:source r))) (assoc :source src-name)
                            bark-path                        (assoc :bark-path bark-path)))
                        reports)}))))

(defn- inject-base-dir
  "Inject :base-dir into each report from a source path."
  [result src]
  (if-let [base (source-base-dir src)]
    (update result :reports (partial mapv #(assoc % :base-dir base)))
    result))

(defn- load-from-file [path]
  (when-not (.exists (io/file path))
    (throw (ex-info (str "File not found: " path) {:path path})))
  (inject-base-dir (unwrap-envelope (load-json-string (slurp path))) path))

(defn- load-from-url [url]
  (unwrap-envelope (load-json-string (fetch-source-body url))))

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
  (let [safe (str/replace src #"[^a-zA-Z0-9._-]" "_")]
    (str reports-cache-dir "/" (subs safe 0 (min 200 (count safe))) ".json")))

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
  (if-let [cached (cache-read src)]
    (inject-base-dir (unwrap-envelope cached) src)
    (let [body (fetch-source-body src)]
      (cache-write! src body)
      (inject-base-dir (unwrap-envelope (load-json-string body)) src))))

(defn- update-sources-cache!
  "Fetch all configured sources and update cache."
  []
  (let [sources (load-sources)]
    (when (empty? sources)
      (throw (ex-info "No sources configured." {})))
    (doseq [src sources]
      (try
        (cache-write! src (fetch-source-body src))
        (println (str "  Updated: " src))
        (catch Exception e
          (binding [*out* *err*]
            (println (str "  [warn] Failed to update " src ": " (.getMessage e)))))))))

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
     (keep (fn [src]
             (try
               (load-one-source-cached src)
               (catch Exception e
                 (binding [*out* *err*]
                   (println (str "  [warn] Failed to load " src ": " (.getMessage e))))
                 nil)))
           sources))))

;; ---------------------------------------------------------------------------
;; Filtering
;; ---------------------------------------------------------------------------

(defn- involves-email?
  "True when the report involves the given email address."
  [report email]
  (let [e (str/lower-case email)]
    (some #(when % (= (str/lower-case %) e))
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

(defn- deadline-days
  "Days from now to the report's :deadline (yyyy-mm-dd).
  Negative = past deadline. Returns nil when no deadline."
  [report]
  (when-let [dl (:deadline report)]
    (try
      (.between java.time.temporal.ChronoUnit/DAYS
                (java.time.LocalDate/now)
                (java.time.LocalDate/parse dl))
      (catch Exception _ nil))))

(defn- deadline-col
  "Format the deadline column: days as string, or empty."
  [report]
  (if-let [d (deadline-days report)] (str d) ""))

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
                 (cond (= cr "canceled") "C"
                       (= cr "resolved") "R"
                       (= cr "expired")  "E"
                       c?                "R"
                       :else             "-"))
     :score (+ (if a? 1 0) (if o? 2 0) (if c? 0 4))}))

(defn- report-columns
  "Return a vector of column values for a report."
  [report show-type? show-src?]
  (concat
   (when show-type? [(:type report "")])
   (when show-src?  [(truncate (:source report "") 10)])
   [(str (:priority report 0))
    (deadline-col report)
    (:flags (report-flags+score report))
    (str (:replies report 0))
    (:from report "?")
    (date-only (:date report))
    (str (vote-cookie report) (:subject report "(no subject)"))]))

(defn- report->row
  "Format a report as a tab-separated row for fzf display."
  [report show-type? show-src?]
  (str/join "\t" (report-columns report show-type? show-src?)))

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
                       (when-let [s (:series report)]
                         (str "series:" (:received s) "/" (:expected s)
                              (when (:closed s) " closed")))
                       (when-let [related (seq (:related report))]
                         (str "→" (str/join "," (distinct (map :type related)))))])]
    (when (seq parts) (str/join " " parts))))

(defn- report->line
  "Format a report as a plain text line."
  [report show-type? show-src?]
  (let [line (str/join "  " (report-columns report show-type? show-src?))]
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
  {"w3m"   ["w3m" "-o" "confirm_qq=false"]
   "lynx"  ["lynx"]
   "links" ["links"]})

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
        (text-browser-cmds b))
      (some (fn [[cmd v]] (when (command-available? cmd) v))
            text-browser-cmds)
      (platform-opener)))

(defn- patch-paths
  "Return a vector of {:url ... :cache-path ...} for each patch in a report.
  Resolves URLs from the report's :base-dir or :bark-path, cache paths from :source."
  [report]
  (when-let [ps (seq (:patches report))]
    (let [base       (or (:base-dir report)
                         (when-let [bp (:bark-path report)]
                           (if (str/ends-with? bp "/") bp (str bp "/"))))
          src-name   (or (:source report) "default")]
      (mapv (fn [p]
              (let [file (:file p)
                    ;; Strip leading "../" segments so cache stays flat under patches-cache-dir
                    cache-file (str/replace-first file #"^(\.\./?)+" "")]
                {:url        (if base (str base "patches/" file) file)
                 :cache-path (str patches-cache-dir "/" src-name "/" cache-file)
                 :filename   (:patch/filename p (:file p))}))
            ps))))

(def ^:private diff-pager-cmds
  {"delta"          ["delta" "--paging" "always"]
   "bat"            ["bat" "--style=plain" "--language=diff" "--paging=always"]
   "diff-so-fancy"  ["diff-so-fancy"]})

(def ^:private stdin-diff-pagers
  "Pagers that read from stdin rather than a file argument."
  #{"delta" "diff-so-fancy"})

(defn- patch-pager
  "Return a command vector for viewing patches with syntax highlighting.
  Honors :diff-pager from config, then probes delta/bat, falls back to $PAGER/less."
  [config]
  (or (when-let [p (:diff-pager config)]
        (diff-pager-cmds p))
      (some (fn [[cmd v]] (when (command-available? cmd) v))
            (dissoc diff-pager-cmds "diff-so-fancy"))
      [(or (System/getenv "PAGER") "less")]))

;; ---------------------------------------------------------------------------
;; Sorting
;; ---------------------------------------------------------------------------

(defn- parse-date-ms
  "Parse a date-raw string to epoch millis for sorting. Returns 0 on failure."
  [s]
  (if (and s (not (str/blank? s)))
    (try
      ;; Java Date.toString() format: "Sat Mar 07 04:10:11 CET 2026"
      (let [fmt (java.text.SimpleDateFormat. "EEE MMM dd HH:mm:ss z yyyy"
                                             java.util.Locale/ENGLISH)]
        (.getTime (.parse fmt s)))
      (catch Exception _
        ;; Try ISO-ish format: "2026-03-07..."
        (try
          (.toEpochMilli (java.time.Instant/parse s))
          (catch Exception _ 0))))
    0))

(def sort-options
  [["date (newest)"    (fn [r] (- (parse-date-ms (:date-raw r))))               compare]
   ["date (oldest)"    (fn [r] (parse-date-ms (:date-raw r)))                   compare]
   ["priority (high)"  (fn [r] (- (:priority r 0)))                              compare]
   ["status (active)"  (fn [r] (- (:score (report-flags+score r))))              compare]
   ["deadline (soon)"  (fn [r] (or (deadline-days r) Integer/MAX_VALUE))          compare]
   ["replies (most)"   (fn [r] (- (:replies r 0)))                               compare]
   ["votes"            (fn [r] (let [[sum total] (parse-votes (:votes r))]
                                 (if (pos? total) (- (/ (double sum) total)) 0.0)))
    compare]
   ["type"             (fn [r] (:type r ""))                                     compare]
   ["author"           (fn [r] (str/lower-case (:from r "")))                    compare]])

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
             "  Ctrl-v                     View patch (fetched to cache)"
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
  The script takes two args: ACTION (open|patch|browse) and LINE_NUMBER.
  Patches are fetched lazily on first view."
  [dispatch-path config visible]
  (let [browse  (browse-cmd config)
        pager   (patch-pager config)
        stdin?  (stdin-diff-pagers (first pager))
        dsf?    (= "diff-so-fancy" (first pager))
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
            ps   (patch-paths report)]
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
        ;; patch action — lazy fetch + page
        (when (seq ps)
          (.append sb (str "  patch:" n ")\n"))
          (if (= 1 (count ps))
            (let [{purl :url cp :cache-path} (first ps)]
              (.append sb (str "    bone_fetch " (shell-escape purl) " " (shell-escape cp) "\n"))
              (.append sb (str "    " (page-cmd-str pager stdin? dsf? (shell-escape cp)) "\n")))
            ;; Multiple patches — let user pick via fzf, then fetch + page
            (let [labels (mapv (fn [{:keys [url]}] (last (str/split url #"/"))) ps)]
              (.append sb (str "    PATCH=$(printf "
                               (shell-escape (str/join "\\n" labels))
                               " | fzf --prompt 'patch> ' --no-sort --reverse)\n"))
              (.append sb "    [ -z \"$PATCH\" ] && exit 0\n")
              (.append sb "    case \"$PATCH\" in\n")
              (doseq [[label {purl :url cp :cache-path}] (map vector labels ps)]
                (.append sb (str "      " (shell-escape label) ")\n"))
                (.append sb (str "        bone_fetch " (shell-escape purl) " " (shell-escape cp) "\n"))
                (.append sb (str "        " (page-cmd-str pager stdin? dsf? (shell-escape cp)) "\n"))
                (.append sb "        ;;\n"))
              (.append sb "    esac\n")))
          (.append sb "    ;;\n"))))
    (.append sb "esac\n")
    (spit dispatch-path (str sb))
    (.setExecutable (io/file dispatch-path) true)
    dispatch-path))

;; ---------------------------------------------------------------------------
;; Display
;; ---------------------------------------------------------------------------

(def ^:private loop-keys
  #{"ctrl-s" "ctrl-r" "ctrl-b" "ctrl-t" "ctrl-x" "ctrl-u"})

(defn- filter-visible
  "Apply active type/source/topic filters to reports."
  [reports {:keys [types sources topics]}
   all-types all-sources all-topics]
  (let [active-types   (or types (set all-types))
        active-sources (or sources (set all-sources))
        active-topics  (or topics (set all-topics))]
    (->> reports
         (filter #(contains? active-types (:type %)))
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

(defn- handle-fzf-key
  "Handle an fzf --expect key. Returns updated state."
  [key-used state all-types all-sources all-topics reload-fn]
  (let [{:keys [reports sort-idx]} state]
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
      "ctrl-u" (if reload-fn
                 (do (println "  Updating cache...")
                     (when (fs/exists? patches-cache-dir)
                       (fs/delete-tree patches-cache-dir))
                     (update-sources-cache!)
                     (let [{new-reports :reports} (reload-fn)]
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
  [config reports & {:keys [reload-fn]}]
  (let [show-type? true
        show-src?  (multiple-sources? reports)
        dispatch-path (str (System/getProperty "java.io.tmpdir")
                           "/bone-dispatch-" (System/currentTimeMillis) ".sh")]
    (if (empty? reports)
      (println "No reports found.")
      (if (fzf-available?)
        (try
          (loop [{:keys [reports sort-idx] :as state}
                 {:reports reports :sort-idx 0 :types nil :sources nil :topics nil}]
            (let [all-types   (vec (distinct (map :type reports)))
                  all-sources (vec (distinct (keep :source reports)))
                  all-topics  (vec (distinct (keep :topic reports)))
                  visible     (filter-visible reports state all-types all-sources all-topics)
                  header      (str/join "\t"
                                        (concat
                                         (when show-type? ["Type"])
                                         (when show-src?  ["Source"])
                                         ["P" "D" "Flags" "#" "From" "Date" "Subject"]))
                  rows        (mapv #(report->row % show-type? show-src?) visible)
                  aligned     (tabulate (cons header rows))
                  input       (str/join "\n" aligned)
                  _           (write-dispatch-script! dispatch-path config visible)
                  {:keys [exit out]}
                  (process/shell {:in input :out :string :continue true}
                                 "fzf" "--header-lines" "1"
                                 "--header" (status-header sort-idx state
                                                           all-types all-sources all-topics)
                                 "--no-sort" "--reverse" "--no-hscroll"
                                 "--prompt" "report> "
                                 "--expect" (str/join "," loop-keys)
                                 "--bind" (str "enter:execute(" dispatch-path " open {n})")
                                 "--bind" (str "ctrl-o:execute-silent(" dispatch-path " browse {n})")
                                 "--bind" (str "ctrl-v:execute(" dispatch-path " patch {n})")
                                 "--bind" (str "ctrl-h:execute(" dispatch-path " help)")
                                 "--bind" "ctrl-n:down"
                                 "--bind" "ctrl-p:up")]
              (let [lines    (when (seq (str/trim out))
                               (str/split-lines (str/trim out)))
                    key-used (first lines)]
                (when (or (zero? exit) (loop-keys key-used))
                  (recur (handle-fzf-key key-used state
                                         all-types all-sources all-topics
                                         reload-fn))))))
          (finally
            (.delete (io/file dispatch-path))))
        ;; Plain text fallback
        (do (println (count reports) "report(s):\n")
            (doseq [r reports]
              (println " " (report->line r show-type? show-src?))))))))

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
                (#{"-s" "--min-score"} a)       (recur (assoc opts :min-score (parse-long (first more))) (rest more))
                (#{"-m" "--mine"} a)            (recur (assoc opts :mine? true) more)
                (#{"-c" "--closed"} a)          (recur (assoc opts :closed? true) more)
                :else                           [opts remaining]))
            email        (or (:email opts) (:email config))
            mine?        (:mine? opts)
            closed?      (:closed? opts)
            min-priority (:min-priority opts)
            min-score    (:min-score opts)]
        (when (and mine? (not email))
          (throw (ex-info "No email configured. Set :email in ~/.config/bone/config.edn or use -e EMAIL." {})))
        (when (and min-priority (not (#{1 2 3} min-priority)))
          (throw (ex-info (str "Invalid --min-priority: " min-priority " (must be 1, 2, or 3)") {})))
        (when (and min-score (not (<= 0 min-score 7)))
          (throw (ex-info (str "Invalid --min-score: " min-score " (must be 0–7)") {})))
        (let [data-src    (:data-src opts)
              src-filter  (:source-filter opts)
              apply-filters
              (fn [{:keys [reports]}]
                {:reports (cond->> reports
                            src-filter              (filter #(= (:source %) src-filter))
                            (and mine? email)       (filter #(involves-email? % email))
                            min-priority            (filter #(>= (:priority % 0) min-priority))
                            min-score               (filter #(>= (:score (report-flags+score %)) min-score))
                            (not closed?)           (remove :closed))})
              raw-result
              (case data-src
                :file      (load-from-file (:path opts))
                :url       (load-from-url (:url opts))
                :urls-file (load-from-urls-file (:urls-path opts))
                :stdin     (load-from-stdin)
                ;; No explicit source → read from sources.json
                (load-from-sources))
              {:keys [reports]} (apply-filters raw-result)
              reload-fn (when (nil? data-src)
                          #(apply-filters (load-from-sources)))]
          (display-reports! config reports :reload-fn reload-fn))))))

(when (= *file* (System/getProperty "babashka.file"))
  (try
    (apply -main *command-line-args*)
    (catch clojure.lang.ExceptionInfo e
      (binding [*out* *err*]
        (println (.getMessage e)))
      (System/exit 1))))
