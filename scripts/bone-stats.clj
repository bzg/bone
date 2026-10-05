#!/usr/bin/env bb

;; bone-stats.clj -- Compute statistics from BONE data.
;;
;; Usage (normally invoked per source by bone-export.clj):
;;   bb scripts/bone-stats.clj json -o <reports-dir>/stats.json -n <source>
;;   bb scripts/bone-stats.clj html -o <base-dir>/data.html -n <source>
;;
;; -o and -n are required (the public/<source>/... layout means there
;; is no single sensible default).  --dir defaults to the directory
;; of -o.
;;
;; Environment / defaults:
;;   BONE_DB -- path to db (default: ./data/bone-db)

(require '[cheshire.core :as json]
         '[clojure.string :as str]
         '[clojure.java.io :as io]
         '[hiccup2.core :as h]
         '[taoensso.timbre :as log]
         '[bone.common :refer [parse-cli-args load-config load-mailmap db-path bone-schema
                               votes-by-report vote-counts]]
         '[bone.common-bb :refer [load-datalevin-pod! dq get-tenures]]
         '[bone.html-bb :refer [set-theme! page-title
                                bone-footer wrap-js spit-html theme-toggle-js
                                nav-bar html-head wrap-template
                                parse-org-table]])

(load-datalevin-pod!)

;; ---------------------------------------------------------------------------
;; JS (loaded from resources/ at build time)
;; ---------------------------------------------------------------------------

(def ^:private stats-js (slurp "resources/bone-stats.js"))

;; db-path is resolved lazily inside generate-json! / generate-html!
;; so loading this namespace doesn't force a config read.

;; ---------------------------------------------------------------------------
;; Time helpers
;; ---------------------------------------------------------------------------

(defn now-ms [] (System/currentTimeMillis))
(def one-year-ms (* 365 24 60 60 1000))

(defn within-last-year? [inst]
  (when inst (> (.getTime inst) (- (now-ms) one-year-ms))))

;; days-between (integer, shared) is in bone.common.
;; This variant returns fractional days for statistical averaging.
(defn- days-between-frac [a b]
  (when (and a b)
    (/ (Math/abs (- (.getTime b) (.getTime a)))
       (* 24.0 60 60 1000))))

(defn round2 [x]
  (when x (/ (Math/round (* x 100.0)) 100.0)))

;; ---------------------------------------------------------------------------
;; Queries
;; ---------------------------------------------------------------------------

(defn emails-last-year
  "Count emails of `source-name` sent in the last year (stats.json is
  per-source; a global count would skew the report/email ratio)."
  [db source-name]
  (let [threshold (java.util.Date. (- (System/currentTimeMillis) one-year-ms))]
    (-> (d/q '[:find (count ?e)
               :in $ ?threshold ?src
               :where
               [?e :email/source ?src]
               [?e :email/message-id _]
               [?e :email/date-sent ?date]
               [(>= ?date ?threshold)]]
             db threshold source-name)
        ffirst (or 0))))

(defn all-participants
  "Fetch all participant entities from the database."
  [db]
  (d/q '[:find ?email ?source ?since
         :where
         [?e :participant/email ?email]
         [?e :participant/source ?source]
         [?e :participant/since ?since]]
       db))

(defn all-contributors
  "Fetch participants who have submitted code (contributor-since is set)."
  [db]
  (d/q '[:find ?email ?source ?since
         :where
         [?e :participant/email ?email]
         [?e :participant/source ?source]
         [?e :participant/contributor-since ?since]]
       db))

(defn total-maintainers
  "Count distinct currently-active maintainer addresses."
  [tenures]
  (->> tenures
       (keep :email)
       distinct
       count))

(defn- maintainers-without-since
  "Count currently-active tenures with no :from (seeded 'forever')."
  [tenures]
  (->> tenures
       (filter #(nil? (:from %)))
       count))

;; ---------------------------------------------------------------------------
;; Stat helpers
;; ---------------------------------------------------------------------------

(defn report-date   [r] (get-in r [:report/email :email/date-sent]))
(defn report-author [r] (get-in r [:report/email :email/author-address]))

(defn open->close-days [r]
  (days-between-frac (report-date r)
                     (get-in r [:report/closed :email/date-sent])))

(defn median [sorted-nums]
  (let [n (count sorted-nums)]
    (when (pos? n)
      (if (odd? n)
        (nth sorted-nums (quot n 2))
        (/ (+ (nth sorted-nums (quot n 2))
              (nth sorted-nums (dec (quot n 2))))
           2.0)))))

;; ---------------------------------------------------------------------------
;; Stat computations
;; ---------------------------------------------------------------------------

(defn reports-per-type [reports]
  (->> reports
       (filter #(within-last-year? (report-date %)))
       (group-by #(some-> (:report/type %) name))
       (into {} (map (fn [[t rs]] [t (count rs)])))))

(def ^:private ym-formatter
  ;; DateTimeFormatter is immutable and thread-safe, so it can be shared.
  (-> (java.time.format.DateTimeFormatter/ofPattern "yyyy-MM")
      (.withLocale java.util.Locale/ENGLISH)
      (.withZone java.time.ZoneOffset/UTC)))

(defn- date->ym
  "Extract 'yyyy-MM' from a java.util.Date using a locale-independent formatter.
  Returns nil on failure."
  [date]
  (when date
    (try (.format ym-formatter (.toInstant ^java.util.Date date))
         (catch Exception _ nil))))

(defn- current-ym
  "Return [year month] for right now, locale-independent."
  []
  (let [s (.format ym-formatter (.toInstant (java.util.Date.)))
        [year month] (str/split s #"-")]
    [(parse-long year) (parse-long month)]))

(def ^:private last-12-months*
  ;; Memoize: the script runs once per invocation, so freezing the
  ;; window at first deref is fine and avoids recomputing 4+ times.
  (delay
   (let [[cy cm] (current-ym)]
     (vec (for [i (range 11 -1 -1)]
            (let [total (+ (* cy 12) (dec cm) (- i))]
              (format "%04d-%02d" (quot total 12) (inc (mod total 12)))))))))

(defn- last-12-months
  "Return a vector of 12 \"yyyy-MM\" strings ending with the current month."
  []
  @last-12-months*)

(defn- cumulative-by-month
  "Given a {\"yyyy-MM\" count} frequency map and a base count (entries before
  the 12-month window), return [[month cumulative] ...] over the last 12 months."
  [by-ym base-count]
  (let [months      (last-12-months)
        first-month (first months)
        base        (+ base-count
                       (->> by-ym
                            (filter (fn [[ym _]] (neg? (compare ym first-month))))
                            (map val)
                            (reduce + 0)))
        cumulative  (rest (reductions + base (map #(get by-ym % 0) months)))]
    (mapv vector months cumulative)))

(defn reports-by-month [reports]
  (let [counts (->> reports
                    (keep (fn [r] (date->ym (report-date r))))
                    frequencies)
        months (last-12-months)]
    (mapv (fn [m] [m (get counts m 0)]) months)))

(defn cumulative-since
  "Cumulative head count per month over the last 12 months, from the
  dates (java.util.Date) at which people joined.  `base` counts the
  people with no known date -- they are present in every month."
  [dates base]
  (cumulative-by-month (frequencies (keep date->ym dates)) base))

(defn email-vs-reports-ratio [reports emails-last-year-count]
  (let [n (count (filter #(within-last-year? (report-date %)) reports))]
    {:reports-last-year n
     :emails-last-year  emails-last-year-count
     :ratio             (when (pos? emails-last-year-count)
                          (round2 (/ n (double emails-last-year-count))))}))

(defn time-to-close-stats [reports]
  (let [durations (->> reports
                       (filter #(and (:report/closed %) (report-date %)))
                       (map open->close-days)
                       (remove nil?)
                       sort vec)]
    (when (seq durations)
      {:count          (count durations)
       :min-days       (round2 (first durations))
       :max-days       (round2 (last durations))
       :avg-days       (round2 (/ (reduce + durations) (count durations)))
       :median-days    (round2 (median durations))
       :buckets        {:same-day       (count (filter #(<  % 1)    durations))
                        :within-week    (count (filter #(and (>= % 1)  (< % 7))   durations))
                        :within-month   (count (filter #(and (>= % 7)  (< % 30))  durations))
                        :within-quarter (count (filter #(and (>= % 30) (<= % 90)) durations))
                        :longer         (count (filter #(>  % 90)    durations))}})))

(defn top-openers
  "Top N openers in the last 12 months.
  Addresses are normalized to lower-case to collapse case-only variants
  (e.g. `Morgan.J.Smith@...` vs `morgan.j.smith@...`).  When `mailmap`
  (`{email-lc -> canonical-name}`) maps the address, the canonical name
  is used both as the group key and as the displayed `:name`, so a
  single contributor posting from multiple addresses appears once."
  [reports n mailmap]
  (->> reports
       (filter #(within-last-year? (report-date %)))
       (map (fn [r]
              (let [addr-lc (some-> (report-author r) str/lower-case)
                    canon   (get mailmap addr-lc)]
                {:lc-addr addr-lc
                 :canon   canon
                 :name    (get-in r [:report/email :email/author-name])})))
       (group-by (fn [{:keys [canon lc-addr]}] (or canon lc-addr)))
       (map (fn [[_ rs]]
              {:address (:lc-addr (first rs))
               :name    (or (:canon (first rs))
                            (some #(when (seq (:name %)) (:name %)) rs))
               :count   (count rs)}))
       (sort-by :count >) (take n)))

(defn open-closed-ratio [reports]
  (let [open     (count (remove :report/closed reports))
        closed   (count (filter :report/closed reports))]
    {:open open :closed closed
     :ratio (when (pos? (+ open closed))
              (round2 (/ open (double (+ open closed)))))}))

(defn closed-cancel-breakdown
  "Among closed reports, count canceled, expired, and resolved per type."
  [reports]
  (let [closed (filter :report/closed reports)]
    (->> closed
         (group-by #(some-> (:report/type %) name))
         (into {}
               (map (fn [[t rs]]
                      (let [canceled (count (filter #(= :canceled (:report/close-reason %)) rs))
                            expired  (count (filter #(= :expired (:report/close-reason %)) rs))
                            resolved (- (count rs) canceled expired)]
                        [t {:canceled canceled :expired expired :resolved resolved}])))))))

(defn vote-leaders [reports all-votes n]
  (->> reports
       (keep (fn [r]
               (when-let [votes (seq (get all-votes (:db/id r)))]
                 (let [{:keys [up down null]} (vote-counts votes)]
                   (when (pos? (+ up down null))
                     {:message-id (:report/message-id r)
                      :topic      (get-in r [:report/email :email/subject])
                      :votes-up   up
                      :votes-down down
                      :votes-null null
                      :score      (- up down)})))))
       (sort-by :score >) (take n)))

(defn compute-stats
  [reports db source-name]
  (let [last-year    (filter #(within-last-year? (report-date %)) reports)
        this-source? #(= source-name (second %))
        contributors (filter this-source? (all-contributors db))
        participants (filter this-source? (all-participants db))
        ;; Currently-active tenures (no :to).
        tenures      (remove :to (get-tenures db source-name))
        all-votes    (votes-by-report
                      (d/q '[:find ?r ?val ?voter ?emid
                             :where
                             [?v :vote/report ?r]
                             [?v :vote/value  ?val]
                             [?v :vote/voter  ?voter]
                             [?v :vote/email  ?e]
                             [?e :email/message-id ?emid]]
                           db))]
    {:generated-at          (str (java.util.Date.))
     :reports-per-type      (reports-per-type reports)
     :reports-by-month      (reports-by-month reports)
     :time-to-close         (time-to-close-stats reports)
     :open-closed-ratio     (open-closed-ratio reports)
     :open-last-year        (count (remove :report/closed last-year))
     :total-last-year       (count last-year)
     :top-openers           (top-openers reports 10 (load-mailmap))
     :vote-leaders          (vote-leaders reports all-votes 10)
     :closed-cancel         (closed-cancel-breakdown reports)
     :email-ratio           (email-vs-reports-ratio
                             reports (emails-last-year db source-name))
     :contributors-by-month (cumulative-since (map #(nth % 2) contributors) 0)
     :total-contributors    (count contributors)
     :participants-by-month (cumulative-since (map #(nth % 2) participants) 0)
     :total-participants    (count participants)
     :total-maintainers     (total-maintainers tenures)
     :maintainers-by-month  (cumulative-since
                             (keep :from tenures)
                             (maintainers-without-since tenures))}))

;; ---------------------------------------------------------------------------
;; HTML / Vega-Lite rendering
;; ---------------------------------------------------------------------------

(defn vl [title mark data encoding & [extra]]
  (merge {:$schema  "https://vega.github.io/schema/vega-lite/v5.json"
          :title    title
          :width    "container"
          :mark     (merge {:type mark :tooltip true} (when (= mark "bar") {:cornerRadiusEnd 3}))
          :data     {:values data}
          :encoding encoding}
         extra))

(def stats-css (slurp "resources/bone-data.css"))

;; Individual chart specs

(defn chart-by-month [by-month]
  (vl "Reports per month (last 12 months)" "bar"
      (vec (map (fn [[m c]] {"month" m "count" c}) by-month))
      {:x {:field "month" :type "ordinal" :sort "ascending"
           :axis  {:labelAngle -45}}
       :y {:field "count" :type "quantitative" :title "Reports"}}))

(defn chart-per-type [per-type]
  (vl "Report types (last 12 months)" "arc"
      (map (fn [[t c]] {"type" t "count" c}) per-type)
      {:theta {:field "count" :type "quantitative"}
       :color {:field "type"  :type "nominal" :title "Type"}}
      {:height 220}))

(defn chart-ttc [ttc]
  (let [order ["same-day" "≤1 week" "≤1 month" "≤3 months" ">3 months"]
        {:keys [same-day within-week within-month within-quarter longer]} (:buckets ttc)
        data  [{"b" "same-day"   "n" same-day}
               {"b" "≤1 week"    "n" within-week}
               {"b" "≤1 month"   "n" within-month}
               {"b" "≤3 months"  "n" within-quarter}
               {"b" ">3 months"  "n" longer}]]
    (vl "Time to close" "bar" data
        {:x {:field "b" :type "ordinal" :title nil :sort order}
         :y {:field "n" :type "quantitative" :title "Reports"}})))

(defn chart-openers [openers]
  ;; Defense in depth: if upstream produced two rows with the same display label
  ;; (e.g. a missing mailmap entry collides two addresses), sum their counts here
  ;; so Vega-Lite never stacks them into one bar with two contradictory tooltips.
  (let [data (->> openers
                  (map (fn [{:keys [address name count]}]
                         {"user" (or (when (seq name) name) address) "count" count}))
                  (group-by #(get % "user"))
                  (map (fn [[u rs]] {"user" u "count" (reduce + (map #(get % "count") rs))}))
                  (sort-by #(get % "count") >))]
    (vl "Top 10 openers (last 12 months)" "bar" data
        {:y {:field "user"  :type "ordinal" :title nil
             :sort {:field "count" :order "descending"}
             :axis  {:labelLimit 180}}
         :x {:field "count" :type "quantitative" :title "Reports opened"}})))

(defn chart-cancel-breakdown [cancel-data]
  (let [data (mapcat (fn [[t {:keys [canceled expired resolved]}]]
                       [{"type" t "reason" "Canceled" "count" canceled}
                        {"type" t "reason" "Expired"  "count" expired}
                        {"type" t "reason" "Resolved" "count" resolved}])
                     cancel-data)]
    (vl "Closed reports: canceled, expired & resolved" "bar" (vec data)
        {:x       {:field "type"   :type "nominal"      :title "Type"}
         :y       {:field "count"  :type "quantitative"  :title "Reports"}
         :color   {:field "reason" :type "nominal"       :title "Close reason"}
         :xOffset {:field "reason"}})))

(defn chart-people
  "Participants, contributors & maintainers line chart with separate lines and legend."
  [participants-by-month contributors-by-month maintainers-by-month]
  (let [partic-data  (mapv (fn [[m c]] {"month" m "count" c "role" "Participants"})
                           participants-by-month)
        contrib-data (mapv (fn [[m c]] {"month" m "count" c "role" "Contributors"})
                           contributors-by-month)
        maint-data   (mapv (fn [[m c]] {"month" m "count" c "role" "Maintainers"})
                           maintainers-by-month)
        data         (into [] cat [partic-data contrib-data maint-data])]
    {:$schema  "https://vega.github.io/schema/vega-lite/v5.json"
     :title    "Participants, contributors & maintainers (last 12 months)"
     :width    "container"
     :data     {:values data}
     :mark     {:type "line" :point true :tooltip true}
     :encoding {:x     {:field "month" :type "ordinal" :sort "ascending"
                        :axis {:labelAngle -45}}
                :y     {:field "count" :type "quantitative"
                        :title "People"}
                :color {:field "role" :type "nominal" :title "Role"
                        :scale {:range ["#72b362" "#4c78a8" "#e45756"]}}}}))

;; ---------------------------------------------------------------------------
;; data.org rendering
;; ---------------------------------------------------------------------------

;; parse-org-table is provided by bone.html-bb (shared with bone-docs).

(defn- strip-dead-data-links
  "Remove individual org links [[target][label]] whose file does not exist.
  Cleans up leftover separators (', ')."
  [line out-dir]
  (let [replaced (str/replace
                  line
                  #"\[\[([^\]]+)\]\[([^\]]+)\]\]"
                  (fn [[whole target _label]]
                    (if (.exists (io/file out-dir target))
                      whole
                      "")))
        ;; Clean up separators left by removed links
        cleaned (-> replaced
                    (str/replace #",\s*,+" ",")
                    (str/replace #"\|\s*," "| ")
                    (str/replace #",\s*\|" " |")
                    ;; Clean cells that only have commas/spaces left
                    (str/replace #"\|\s*,\s*\|" "| |")
                    (str/replace #"\|\s+,\s*$" "| "))]
    cleaned))

(defn render-data-section
  "Render resources/data.org as an HTML section, filtering dead links.
  A row that had links and lost them all is dropped."
  [out-dir]
  (let [link?    #(re-find #"\[\[" %)
        filtered (->> (str/split-lines (slurp "resources/data.org"))
                      (filter #(str/starts-with? (str/trim %) "|"))
                      (keep (fn [line]
                              (if (link? line)
                                (let [stripped (strip-dead-data-links line out-dir)]
                                  (when (link? stripped) stripped))
                                line))))]
    (when (seq filtered)
      (str "<h3>Available data</h3>\n"
           (parse-org-table filtered)))))

;; ---------------------------------------------------------------------------
;; HTML assembly
;; ---------------------------------------------------------------------------

;; View model: KPIs and chart specs, derived from the computed stats.
;; These are written into stats.json (alongside the raw numbers) and the
;; client builds the DOM from them, so data.html itself carries no data.

(defn view-kpis
  "Ordered KPI cards as {:v value :l label :s sub?} maps, nil-filtered."
  [stats]
  (let [{:keys [reports-per-type open-closed-ratio open-last-year time-to-close
                email-ratio total-participants total-contributors total-maintainers]} stats
        pct  #(when % (str (Math/round (* 100.0 %)) "%"))
        n-yr (reduce + (vals reports-per-type))]
    (filterv some?
      [{:v n-yr :l "Reports (last 12 months)" :s (str open-last-year " still open")}
       {:v (:open open-closed-ratio) :l "Open (all time)"
        :s (str (pct (:ratio open-closed-ratio)) " of all")}
       {:v (:closed open-closed-ratio) :l "Closed (all time)"}
       (when time-to-close
         {:v (str (:median-days time-to-close) "d") :l "Median to close"
          :s (str "avg " (:avg-days time-to-close) "d")})
       {:v (or (:ratio email-ratio) "--") :l "Report/email ratio (last 12 months)"
        :s (str (:reports-last-year email-ratio) " reports / "
                (:emails-last-year email-ratio) " emails")}
       {:v total-participants :l "Participants"
        :s (str total-contributors " contributors")}
       {:v total-maintainers :l "Maintainers"}])))

(defn view-charts
  "Ordered Vega-Lite chart specs as {:id dom-id :spec spec} maps,
  nil-filtered."
  [stats]
  (let [{:keys [reports-per-type reports-by-month time-to-close top-openers
                closed-cancel participants-by-month contributors-by-month
                maintainers-by-month]} stats]
    (filterv some?
      [{:id "chart-month"   :spec (chart-by-month reports-by-month)}
       {:id "chart-type"    :spec (chart-per-type reports-per-type)}
       {:id "chart-people" :spec (chart-people participants-by-month
                                               contributors-by-month
                                               maintainers-by-month)}
       (when time-to-close
         {:id "chart-ttc"    :spec (chart-ttc time-to-close)})
       {:id "chart-openers" :spec (chart-openers top-openers)}
       (when (seq closed-cancel)
         {:id "chart-cancel" :spec (chart-cancel-breakdown closed-cancel)})])))

(defn render-shell
  "Static data.html shell: nav, empty KPI/chart containers, the JS-less
  \"Available data\" table, and the scripts.  Carries no report data --
  bone-stats.js fetches stats.json (via meta.json) and fills it in, so the
  file stays byte-stable across data-only changes."
  [out-dir source-name {:keys [website]}]
  (let [title        (page-title "Data" source-name)
        has-ical?    (.exists (io/file out-dir "events" "announcements.ics"))
        data-section (render-data-section out-dir)
        vega-scripts (str "<script src=\"https://cdn.jsdelivr.net/npm/vega@5/build/vega.min.js\"></script>\n"
                          "<script src=\"https://cdn.jsdelivr.net/npm/vega-lite@5/build/vega-lite.min.js\"></script>\n"
                          "<script src=\"https://cdn.jsdelivr.net/npm/vega-embed@6/build/vega-embed.min.js\"></script>\n"
                          "<script>\n" (wrap-js theme-toggle-js) "\n</script>\n"
                          "<script>\n" (wrap-js stats-js) "\n</script>\n")
        ;; Injected only when JS runs (wrap-template).  JS-less browsers skip
        ;; <script> bodies, so the page reduces to <noscript><h1></h1></noscript>
        ;; + the Available-data table.
        tpl-nav      (str (h/html (nav-bar (page-title "Data" nil) "data"
                                           {:source source-name
                                            :source-href website}))
                          "\n<p class=\"meta\" id=\"generated-at\"></p>\n")
        tpl-stats    (str "<section class=\"stats-section\">\n"
                          "<h3>Statistics</h3>\n"
                          "<div id=\"kpi-area\" class=\"kpis\"></div>\n"
                          "<div id=\"chart-grid\" class=\"grid\"></div>\n"
                          "</section>\n")
        tpl-footer   (str (h/html (bone-footer {:ical has-ical?
                                                :website website
                                                :source source-name})))]
    (str
     "<!DOCTYPE html>\n"
     "<html lang=\"en\" data-theme=\"light\">\n"
     (html-head {:title      title
                 :css        stats-css
                 :extra-head vega-scripts})
     "<body>\n"
     "<noscript><h1>" title "</h1></noscript>\n"
     "<main class=\"container\">\n"
     (wrap-template "tpl-nav" tpl-nav)
     ;; Available data: always visible -- the only content rendered for JS-less browsers.
     (when data-section (str data-section "\n"))
     (wrap-template "tpl-stats" tpl-stats)
     "</main>\n"
     (wrap-template "tpl-footer" tpl-footer)
     "</body>\n</html>\n")))

;; ---------------------------------------------------------------------------
;; Main
;; ---------------------------------------------------------------------------

(def ^:private stats-pull-pattern
  ;; compute-stats only reads dates, type, close-reason, author and
  ;; subject -- never headers-edn, relations or patches.  A dedicated
  ;; lean pattern keeps the all-reports re-pull cheap at scale, where
  ;; the shared report-pull-pattern would drag headers-edn (pulled
  ;; thrice per report) and every relation sub-tree along for nothing.
  '[:db/id :report/type :report/message-id :report/close-reason
    {:report/closed [:email/date-sent]}
    {:report/email [:email/date-sent :email/author-address
                    :email/author-name :email/subject :email/source]}])

(defn- all-reports-lean
  "Like `all-reports` but with `stats-pull-pattern`."
  [db]
  (->> (dq (list :find (list 'pull '?r stats-pull-pattern)
                 :where ['?r :report/type '_])
           db)
       (map first)))

(defn- generate-json! [out-file source-name]
  (let [conn (d/get-conn (db-path (load-config)) bone-schema {:wal? false})]
    (try
      (let [db       (d/db conn)
            reports  (filter #(= source-name (get-in % [:report/email :email/source]))
                             (all-reports-lean db))
            stats    (compute-stats reports db source-name)
            ;; Bundle the data.html view model (KPI cards + chart specs)
            ;; into stats.json so the data.html shell can render entirely
            ;; client-side; raw stat keys remain for other consumers.
            payload  (assoc stats :kpis   (view-kpis stats)
                                  :charts (view-charts stats))]
        (spit out-file (json/generate-string payload {:pretty true}))
        (log/info "Wrote" out-file "(JSON," (count reports) "reports)"))
      (finally
        (try (d/close conn) (catch Exception _ nil))))))

(defn- generate-html! [out-file out-dir source-name]
  ;; A static shell: no stats are read here.  bone-stats.js fetches
  ;; stats.json (discovered via meta.json) and fills the page in.
  (let [src (some #(when (= source-name (:name %)) %)
                  (:sources (load-config)))]
    (spit-html out-file (render-shell out-dir source-name
                                      (select-keys src [:website]))))
  (log/info "Wrote shell" out-file))

(defn -main [& args]
  (let [opts        (parse-cli-args args)
        _           (when-let [t (:theme opts)] (set-theme! t))
        html?       (= (:format opts) "html")
        source-name (:source-name opts)
        out-file    (:out-file opts)
        _           (when (or (str/blank? out-file) (str/blank? source-name))
                      (log/error "bone-stats.clj requires -o <file> and -n <source>")
                      (System/exit 2))
        ;; getParent is nil for a bare filename: default to ".".
        out-dir     (or (:out-dir opts) (.getParent (io/file out-file)) ".")]
    (io/make-parents out-file)
    (if html?
      (generate-html! out-file out-dir source-name)
      (generate-json! out-file source-name))))
;; Run only when invoked as a script, not when the file is loaded.
(when (= (System/getProperty "babashka.file") *file*)
  (apply -main *command-line-args*))
