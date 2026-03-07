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
;;
;; Options:
;;   -f, --file FILE          Read reports from a JSON file
;;   -u, --url  URL           Fetch reports from a URL
;;   -U, --urls-file FILE     Fetch & merge reports from URLs listed in FILE
;;   -e, --email EMAIL        Your email (overrides config)
;;   -p, --min-priority 1-3   Only show reports with priority >= N
;;   -s, --min-status 1-7     Only show reports with status >= N
;;   -n, --source NAME        Filter by source name
;;   -a, --all                Show all reports (not just yours)
;;   -c, --closed             Include closed reports
;;   -                        Read JSON from stdin
;;
;; By default, bone filters reports to those where your email appears
;; in from, acked-by, owned-by, or closed-by.  Use -a to see all.

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

(defn- load-config []
  (let [f (io/file config-path)]
    (if (.exists f)
      (read-string (slurp f))
      {})))

;; ---------------------------------------------------------------------------
;; Data loading
;; ---------------------------------------------------------------------------

(defn- load-json-string [s]
  (json/parse-string s keyword))

(defn- load-from-file [path]
  (when-not (.exists (io/file path))
    (binding [*out* *err*]
      (println (str "File not found: " path)))
    (System/exit 1))
  (load-json-string (slurp path)))

(defn- load-from-url [url]
  (let [resp (http/get url {:headers {"Accept" "application/json"}})]
    (when (not= 200 (:status resp))
      (binding [*out* *err*]
        (println (str "HTTP " (:status resp) " fetching " url)))
      (System/exit 1))
    (load-json-string (:body resp))))

(defn- load-from-stdin []
  (load-json-string (slurp *in*)))

(defn- load-from-urls-file [path]
  (when-not (.exists (io/file path))
    (binding [*out* *err*]
      (println (str "URLs file not found: " path)))
    (System/exit 1))
  (let [urls (->> (str/split-lines (slurp path))
                  (map str/trim)
                  (remove #(or (str/blank? %) (str/starts-with? % "#"))))]
    (into [] (mapcat #(let [r (load-from-url %)]
                        (if (sequential? r) r [r])))
          urls)))

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

(defn display-reports!
  "Display reports interactively with fzf, or as plain text lines."
  [reports]
  (let [show-type? true
        show-src?   (has-source? reports)]
    (if (empty? reports)
      (println "No reports found.")
      (if (fzf-available?)
        (let [header (str/join "\t"
                               (concat
                                (when show-type? ["Type"])
                                (when show-src?   ["Source"])
                                ["P" "Flags" "#" "From" "Date" "Subject"]))
              rows   (mapv #(report->row % show-type? show-src?) reports)
              aligned      (tabulate (cons header rows))
              header-line  (first aligned)
              aligned-rows (vec (rest aligned))
              input        (str/join "\n" aligned-rows)
              {:keys [exit out]}
              (process/shell {:in input :out :string :continue true}
                             "fzf" "--header" header-line
                             "--no-sort" "--reverse"
                             "--prompt" "report> ")]
          (when (zero? exit)
            (let [selected (str/trim out)
                  idx      (.indexOf ^java.util.List aligned-rows selected)]
              (when (>= idx 0)
                (let [report (nth reports idx)
                      url    (:archived-at report)]
                  (if url
                    (let [os   (str/lower-case (System/getProperty "os.name"))
                          open (cond
                                 (str/includes? os "mac") "open"
                                 (str/includes? os "win") "start"
                                 :else                    "xdg-open")]
                      (process/shell open url))
                    (do (println "No archived-at URL for this report.")
                        (pprint/pprint report))))))))
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
  (println "  -a, --all                  Show all reports (not just yours)")
  (println "  -c, --closed               Include closed reports")
  (println "  -                          Read JSON from stdin")
  (println)
  (println "Config: ~/.config/bone/config.edn  e.g. {:email \"you@example.com\"}"))

(defn -main [& args]
  (let [args   (or (seq args) *command-line-args*)
        config (load-config)]
    (if (or (empty? args) (some #{"-h" "--help"} args))
      (usage)
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
                (#{"-a" "--all"} a)             (recur (assoc opts :all? true) more)
                (#{"-c" "--closed"} a)          (recur (assoc opts :closed? true) more)
                :else                           [opts remaining]))
            email        (or (:email opts) (:email config))
            all?         (:all? opts)
            closed?      (:closed? opts)
            min-priority (:min-priority opts)
            min-status   (:min-status opts)]
        (when (and (not all?) (not email))
          (binding [*out* *err*]
            (println "No email configured. Set :email in ~/.config/bone/config.edn or use -e EMAIL.")
            (println "Use -a/--all to show all reports without filtering."))
          (System/exit 1))
        (when (and min-priority (not (#{1 2 3} min-priority)))
          (binding [*out* *err*] (println (str "Invalid --min-priority: " min-priority " (must be 1, 2, or 3)")))
          (System/exit 1))
        (when (and min-status (not (<= 1 min-status 7)))
          (binding [*out* *err*] (println (str "Invalid --min-status: " min-status " (must be 1–7)")))
          (System/exit 1))
        (let [reports (case (:data-src opts)
                        :file      (load-from-file (:path opts))
                        :url       (load-from-url (:url opts))
                        :urls-file (load-from-urls-file (:urls-path opts))
                        :stdin     (load-from-stdin)
                        (do (binding [*out* *err*]
                              (println "Error: specify -f FILE, -u URL, -U URLS_FILE, or - for stdin."))
                            (System/exit 1)))
              reports (if-let [src (:source-filter opts)]
                        (filter-by-source reports src) reports)
              reports (if (and email (not all?))
                        (filter-mine reports email) reports)
              reports (if min-priority
                        (filter-by-priority reports min-priority) reports)
              reports (if min-status
                        (filter-by-status reports min-status) reports)
              reports (if-not closed?
                        (filter-open reports) reports)]
          (display-reports! reports))))))

(when (= *file* (System/getProperty "babashka.file"))
  (apply -main *command-line-args*))
