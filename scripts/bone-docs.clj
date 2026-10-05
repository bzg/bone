#!/usr/bin/env bb

;; bone-docs.clj -- Generate public/<source>/docs.html from resources/docs-tpl.org.
;;
;; Reads the org template and substitutes source-specific labels and
;; commands into the unified table, based on merged config
;; (defaults -> global -> per-source).
;;
;; Usage:
;;   bb scripts/bone-docs.clj -n my-source -o out.html  -> writes out.html
;;
;; -o is required (the public/<source>/... layout means there is no
;; single sensible default).  --dir defaults to the directory of -o.

(require '[clojure.java.io :as io]
         '[clojure.java.shell]
         '[clojure.string :as str]
         '[hiccup2.core :as h]
         '[taoensso.timbre :as log]
         '[bone.common :refer [default-labels default-commands close-reasons
                               resolve-labels-map resolve-commands-map
                               resolve-command-syntax reproducible-config-str
                               parse-cli-args load-config load-mailmap db-path build-source-map
                               format-date-iso bone-schema lead-maintainer]]
         '[bone.common-bb :refer [load-datalevin-pod! dq get-tenures]]
         '[bone.html-bb :refer [set-theme! head-hiccup
                                footer-css theme-toggle-css bone-footer wrap-js
                                spit-html theme-toggle-js nav-bar
                                org-inline parse-org-table html-escape]])

(load-datalevin-pod!)

;; ---------------------------------------------------------------------------
;; Build the org table from resolved labels + commands
;; ---------------------------------------------------------------------------

(defn- fmt-label-tags
  "Format label tags as org =code= entries for a given report type."
  [tags rtype]
  (let [versioned?  #{:release :change}
        with-topic? #{:bug :request :announcement}]
    (str/join " "
              (cond
                (= rtype :patch)
                (map (fn [t] (str "=[" t " <topic> <version> <n/m>]=")) tags)

                (versioned? rtype)
                (map (fn [t] (str "=[" t " <topic> <version>]=")) tags)

                (with-topic? rtype)
                (map (fn [t] (str "=[" t " <topic>]=")) tags)

                :else
                (map #(str "=[" % "]=") tags)))))

(defn- fmt-command-words
  "Format command words as org =code= entries, prefixed with the
  source's instruction prefix (empty for :loose, '!' for :strict)."
  [words prefix]
  (if (seq words)
    (str/join " " (map #(str "=" prefix % "=") words))
    ""))

(defn build-labels-table-org
  "Build the labels-only org table."
  [labels]
  (let [types-upper [:bug :patch :request]
        types-lower [:announcement :release :change]
        all-types   (concat types-upper types-lower)
        rows  (mapv (fn [rtype]
                      {:type   (name rtype)
                       :labels (fmt-label-tags (get labels rtype) rtype)})
                    all-types)
        w-type   (apply max (count "Type")           (map #(count (:type %)) rows))
        w-labels (apply max (count "Subject labels") (map #(count (:labels %)) rows))
        pad      (fn [s w] (str s (apply str (repeat (max 0 (- w (count s))) " "))))
        hline    (str "|-" (apply str (repeat w-type "-")) "-+-"
                      (apply str (repeat w-labels "-")) "-|")
        row-str  (fn [{:keys [type labels]}]
                   (str "| " (pad type w-type)
                        " | " (pad labels w-labels) " |"))
        header   (row-str {:type "Type" :labels "Subject labels"})
        upper    (map row-str (take 3 rows))
        lower    (map row-str (drop 3 rows))]
    (str/join "\n" (concat [header hline] upper [hline] lower))))

(defn build-commands-table-org
  "Build the merged status+priority commands org table (3 columns).
  `prefix` is the source's instruction prefix (\"\" for :loose, \"!\"
  for :strict)."
  [cmds prefix]
  (let [;; Words mapping to each close reason come from the runtime
        ;; authority (common/close-reasons); anything else resolves.
        reason-words (fn [reason]
                       (set (keep (fn [[w r]] (when (= r reason) w))
                                  close-reasons)))
        canceled     (reason-words :canceled)
        expired      (reason-words :expired)
        resolved?    #(not (contains? (set (keys close-reasons)) %))
        rows [["Mark as acked"             (fmt-command-words (:acked cmds) prefix)  "Status"]
              ["Mark as owned"             (fmt-command-words (:owned cmds) prefix)  "Status"]
              ["Mark as closed (canceled)" (fmt-command-words (filterv canceled (:closed cmds)) prefix) "Status"]
              ["Mark as closed (expired)"  (fmt-command-words (filterv expired (:closed cmds)) prefix) "Status"]
              ["Mark as closed (resolved)" (fmt-command-words (filterv resolved? (:closed cmds)) prefix) "Status"]
              ["Mark as urgent"            (fmt-command-words (:urgent cmds) prefix)    "Priority"]
              ["Mark as important"         (fmt-command-words (:important cmds) prefix) "Priority"]]
        w-effect  (apply max (count "Effect on report")  (map #(count (nth % 0)) rows))
        w-command (apply max (count "Command keyword")   (map #(count (nth % 1)) rows))
        w-type    (apply max (count "Type")              (map #(count (nth % 2)) rows))
        pad       (fn [s w] (str s (apply str (repeat (max 0 (- w (count s))) " "))))
        hline     (str "|-" (apply str (repeat w-effect "-")) "-+-"
                       (apply str (repeat w-command "-")) "-+-"
                       (apply str (repeat w-type "-")) "-|")
        row-str   (fn [[effect command typ]]
                    (str "| " (pad effect w-effect)
                         " | " (pad command w-command)
                         " | " (pad typ w-type) " |"))
        header    (row-str ["Effect on report" "Command keyword" "Type"])]
    (str/join "\n" (concat [header hline] (map row-str rows)))))

;; ---------------------------------------------------------------------------
;; Template substitution -- detect and replace org table blocks
;; ---------------------------------------------------------------------------

(defn- table-line? [s] (str/starts-with? (str/trim s) "|"))

(defn- find-table-blocks
  "Return a seq of [start-idx end-idx] for each contiguous table block."
  [lines]
  (loop [i 0, blocks [], in-table? false, start nil]
    (if (>= i (count lines))
      (if in-table? (conj blocks [start (dec i)]) blocks)
      (if (table-line? (nth lines i))
        (recur (inc i) blocks true (or start i))
        (if in-table?
          (recur (inc i) (conj blocks [start (dec i)]) false nil)
          (recur (inc i) blocks false nil))))))

(defn substitute-template
  "Replace the first two org table blocks in org-text:
  the first with the resolved labels table, the second with the
  commands table.  `prefix` is prepended to every displayed keyword
  (\"\" for :loose, \"!\" for :strict)."
  [org-text labels cmds prefix]
  (let [lines          (str/split-lines org-text)
        blocks         (find-table-blocks lines)
        labels-block   (build-labels-table-org labels)
        commands-block (build-commands-table-org cmds prefix)]
    (cond
      (>= (count blocks) 2)
      (let [[t1-start t1-end] (nth blocks 0)
            [t2-start t2-end] (nth blocks 1)]
        (str/join "\n"
                  (concat (take t1-start lines)
                          [labels-block]
                          (subvec (vec lines) (inc t1-end) t2-start)
                          [commands-block]
                          (drop (inc t2-end) lines))))

      ;; Only one table -- replace with labels only
      (seq blocks)
      (let [[t1-start t1-end] (first blocks)]
        (str/join "\n"
                  (concat (take t1-start lines)
                          [labels-block]
                          (drop (inc t1-end) lines))))

      :else org-text)))

;; ---------------------------------------------------------------------------
;; Version stamp
;; ---------------------------------------------------------------------------

(defn- bone-version
  "BONE version for display in docs.html: `git describe --tags` from
  the checkout running the export (releases are tagged, so this reads
  e.g. \"0.92.6\", or \"0.92.6-3-gabc1234\" between releases).
  Returns nil when git or the repo is unavailable."
  []
  (try
    (let [{:keys [exit out]} (clojure.java.shell/sh "git" "describe" "--tags" "--always")]
      (when (zero? exit)
        (not-empty (str/trim out))))
    (catch Exception _ nil)))

(defn- substitute-version
  "Replace the [version] placeholder with the running BONE version,
  or drop it (with its leading space) when the version is unknown."
  [org-text version]
  (if version
    (str/replace org-text "[version]" version)
    (str/replace org-text " [version]" "")))

(defn- substitute-source-links
  "Replace the [source-links] placeholder with the per-source contact
  paragraph: the posting address (:post-address, falling back to :to
  for mailbox sources) and the :contribute-url page.  Dropped when the
  source configures neither."
  [org-text {:keys [post-address to contribute-url]}]
  (let [addr  (or post-address to)
        parts (cond-> []
                addr
                (conj (str "Reports are created and updated by emailing "
                           "[[mailto:" addr "][" addr "]]."))
                contribute-url
                (conj (str "New to the project? See [["
                           contribute-url "][how to contribute]].")))]
    (str/replace org-text "[source-links]" (str/join "  " parts))))

;; ---------------------------------------------------------------------------
;; Minimal org -> HTML conversion
;; ---------------------------------------------------------------------------

;; org-inline is provided by bone.html-bb (HTML-escape aware).

(defn- heading-id [text]
  (-> text str/lower-case str/trim
                          (str/replace #"[^a-z0-9 -]" "")
                          (str/replace #"\s+" "-")))

;; parse-org-table is provided by bone.html-bb (shared with bone-stats).

(defn org->html [org-text]
  (let [lines (str/split-lines org-text)]
    (loop [i 0, acc (transient []), in-para? false]
      (if (>= i (count lines))
        (let [acc (if in-para? (conj! acc "</p>") acc)]
          (str/join "\n" (persistent! acc)))
        (let [line (nth lines i)
              trimmed (str/trim line)]
          (cond
            (re-matches #"\*+ .+" line)
            (let [[_ stars text] (re-find #"^(\*+) (.+)" line)
                  level (min (count stars) 6)
                  text  (str/replace text #"\s*<<[^>]+>>\s*" "")
                  tag   (str "h" level)
                  id    (heading-id text)
                  acc   (if in-para? (conj! acc "</p>") acc)]
              (recur (inc i)
                     (conj! acc (str "<" tag " id=\"" id "\">" (org-inline text) "</" tag ">"))
                     false))

            (or (= trimmed ":PROPERTIES:")
                (= trimmed ":END:")
                (re-matches #":CUSTOM_ID:.*" trimmed))
            (recur (inc i) acc in-para?)

            (re-matches #"(?i)#\+begin_(?:example|src.*)" trimmed)
            (let [src? (re-matches #"(?i)#\+begin_src.*" trimmed)
                  [open close end-re]
                  (if src?
                    ["<pre><code>" "</code></pre>" #"(?i)#\+end_src"]
                    ["<pre>" "</pre>" #"(?i)#\+end_example"])
                  acc  (if in-para? (conj! acc "</p>") acc)
                  acc  (conj! acc open)
                  next (loop [j (inc i), a acc]
                         (if (>= j (count lines))
                           [j a]
                           (let [bl (str/trim (nth lines j))]
                             (if (re-matches end-re bl)
                               [(inc j) a]
                               (recur (inc j)
                                      (conj! a (-> (nth lines j)
                                                   (str/replace "&" "&amp;")
                                                   (str/replace "<" "&lt;"))))))))]
              (recur (first next) (conj! (second next) close) false))

            (str/starts-with? trimmed "|")
            (let [acc    (if in-para? (conj! acc "</p>") acc)
                  tlines (loop [j i, tl []]
                           (if (and (< j (count lines))
                                    (str/starts-with? (str/trim (nth lines j)) "|"))
                             (recur (inc j) (conj tl (nth lines j)))
                             [j tl]))
                  [next-i table-lines] tlines]
              (recur next-i (conj! acc (parse-org-table table-lines)) false))

            (str/blank? trimmed)
            (let [acc (if in-para? (conj! acc "</p>") acc)]
              (recur (inc i) acc false))

            (str/starts-with? trimmed "#")
            (recur (inc i) acc in-para?)

            :else
            (if in-para?
              (recur (inc i) (conj! acc (org-inline trimmed)) true)
              (recur (inc i) (conj! acc (str "<p>" (org-inline trimmed))) true))))))))

;; ---------------------------------------------------------------------------
;; Page assembly
;; ---------------------------------------------------------------------------

(def docs-css (str "
  main.container { max-width: 1600px; }
  table { font-size: 0.9rem; }
  pre { font-size: 0.85rem; padding: 1rem; }
  " theme-toggle-css "
  .meta { font-size: 0.78rem; color: var(--pico-muted-color); margin-bottom: 2rem; }
" footer-css))

(defn docs-page [body-html {:keys [ical website source]}]
  (let [title        "BONE - Docs"
        generated-at (str (java.util.Date.))]
    (str
     "<!DOCTYPE html>\n"
     (h/html
      [:html {:lang "en" :data-theme "light"}
       (head-hiccup {:title title :css docs-css})
       [:body
        [:main.container
         (nav-bar title "docs")
         [:p.meta (str "Generated " generated-at)]
         (h/raw body-html)
         [:script (h/raw (wrap-js theme-toggle-js))]]
        (bone-footer {:ical ical
                      :website website
                      :source source})]]))))

;; ---------------------------------------------------------------------------
;; Maintainers section
;; ---------------------------------------------------------------------------

(defn- participant-names-for-source
  "Return `{lowercase-email -> non-blank name}` for all participants of
  `source-name`.  Single query, intended for batch lookups."
  [db source-name]
  (->> (dq '[:find ?email ?name
             :in $ ?src
             :where
             [?e :participant/source ?src]
             [?e :participant/email ?email]
             [?e :participant/name ?name]]
           db source-name)
       (reduce (fn [acc [email name]]
                 (if (str/blank? name) acc (assoc acc email name)))
               {})))

(defn build-maintainers-html
  "Build an HTML section listing all maintainer tenures (active and closed),
  with :from/:to dates when known. The lead maintainer is highlighted.
  Identical rendered lines are deduplicated: when several email
  addresses resolve to the same display name (via mailmap.edn) with
  the same date range and lead status, only one line is shown."
  [db source-name]
  (when source-name
    (let [tenures     (get-tenures db source-name)
          lead        (lead-maintainer tenures)
          names       (merge (participant-names-for-source db source-name)
                             (load-mailmap))
          ;; Sort: lead first, then alphabetical by email (case-insensitive).
          sort-key    (fn [{:keys [email to]}]
                        [(if (and (nil? to) (= email lead)) 0 1)
                         (str/lower-case (or email ""))])
          ordered     (sort-by sort-key tenures)
          entries     (mapv
                       (fn [{:keys [email from to]}]
                         (let [display (or (get names (some-> email str/lower-case)) email)
                               escaped (html-escape display)
                               range   (cond
                                         (and from to)
                                         (str " <small>(" (format-date-iso from) " -- " (format-date-iso to) ")</small>")
                                         from
                                         (str " <small>(since " (format-date-iso from) ")</small>")
                                         to
                                         (str " <small>(until " (format-date-iso to) ")</small>")
                                         :else "")
                               badge   (when (and (nil? to) (= email lead))
                                         " <small><em>(lead)</em></small>")]
                           (str escaped range badge)))
                       ordered)
          entries     (vec (distinct entries))]
      (when (seq entries)
        (str "<h2 id=\"maintainers\">Maintainers</h2>\n<ul>\n"
             (str/join "\n" (map #(str "<li>" % "</li>") entries))
             "\n</ul>")))))

(defn build-configuration-html
  "Build the Configuration section: a complete, self-contained config.edn
  the reader drops into their own BONE instance to reproduce this source
  on their own copy of the mail.  Global :labels/:commands/... are folded
  in and secrets/operator-internal keys are dropped (see
  `bone.common/effective-source-config`)."
  [config source-name]
  (when-let [edn (and source-name (reproducible-config-str config source-name))]
    (str "<h2 id=\"configuration\">Configuration</h2>\n"
         "<p>To reproduce this dashboard on your own copy of the mail, run "
         "BONE with this <code>config.edn</code> "
         "(<a href=\"reports/config.edn\">download</a>). Replace "
         "<code>:mailboxes</code> with your own local source -- the operator's "
         "mailbox is private and not needed -- then run <code>bb export</code>.</p>\n"
         "<pre><code>"
         (html-escape (str/trim edn))
         "</code></pre>")))

;; ---------------------------------------------------------------------------
;; Main
;; ---------------------------------------------------------------------------

(let [{:keys [out-file out-dir source-name theme]} (parse-cli-args *command-line-args*)
      config      (load-config)
      _           (set-theme! (or theme (:theme config)))
      source-map  (when config (build-source-map config))
      source-cfg  (get source-map source-name)
      labels      (if source-cfg (resolve-labels-map source-cfg) default-labels)
      cmds        (if source-cfg (resolve-commands-map source-cfg) default-commands)
      prefix      (if (= :strict (resolve-command-syntax source-cfg)) "!" "")
      _           (when (str/blank? out-file)
                    (binding [*out* *err*]
                      (log/error "bone-docs.clj requires -o <file>"))
                    (System/exit 2))
      effective-dir (or out-dir (.getParent (io/file out-file)))
      ;; Load DB for maintainer names
      conn        (d/get-conn (db-path config) bone-schema {:wal? false})
      ;; try/finally so the connection is closed even when the template
      ;; slurp or rendering throws (same pattern as the other scripts).
      html        (try
                    (let [db          (d/db conn)
                          maint-html  (build-maintainers-html db source-name)
                          config-html (build-configuration-html config source-name)
                          org-text    (-> (slurp "resources/docs-tpl.org")
                                          (substitute-version (bone-version))
                                          (substitute-source-links source-cfg)
                                          (substitute-template labels cmds prefix))
                          body-html   (cond-> (org->html org-text)
                                        maint-html  (str "\n" maint-html)
                                        config-html (str "\n" config-html))
                          has-ical?   (.exists (io/file effective-dir "events"
                                                        "announcements.ics"))]
                      (docs-page body-html {:ical has-ical?
                                            :website (:website source-cfg)
                                            :source source-name}))
                    (finally
                      (d/close conn)))]
  (io/make-parents out-file)
  (spit-html out-file html)
  ;; Routine progress on stdout (captured in the export log), not stderr:
  ;; docs.html is now regenerated only on structural changes, and the cron
  ;; notification is driven by an explicit summary line in bone-export.clj.
  (log/info "Wrote" out-file))
