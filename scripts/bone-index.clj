#!/usr/bin/env bb

;; bone-index.clj -- Generate the reports index HTML page from BONE data.
;;
;; Reads reports.json (produced by bone-export) and config.edn, then
;; builds a standalone HTML page.  Most logic is in Clojure; JS is
;; limited to client-side filtering, sorting, theme toggle, and URL
;; permalink state.
;;
;; Usage:
;;   bb export html                              -> via bb task (preferred)
;;   bb scripts/bone-index.clj -o <out.html> --json <reports.json>
;;
;; --json and -o are required (the public/<source>/... layout means
;; there is no single sensible default).  --dir defaults to the
;; directory of --json.

(require '[cheshire.core :as json]
         '[clojure.java.io :as io]
         '[clojure.string :as str]
         '[hiccup2.core :as h]
         '[taoensso.timbre :as log]
         '[bone.common :refer [parse-cli-args escape-script-payload]]
         '[bone.html-bb :refer [pico-cdn resolved-theme set-theme!
                                bone-description page-title
                                footer-css bone-footer wrap-js spit-html
                                noscript-banner wrap-template
                                theme-toggle-js nav-bar]])

;; ---------------------------------------------------------------------------
;; Config
;; ---------------------------------------------------------------------------

(def type-labels {"bug" "bug" "announcement" "ann" "request" "req"
                  "patch" "patch" "release" "rel" "change" "chg"})

;; ---------------------------------------------------------------------------
;; CSS (inlined)
;; ---------------------------------------------------------------------------

(def page-css (str "
  main.container { max-width: 1800px; padding-left: max(1rem, env(safe-area-inset-left)); padding-right: max(1rem, env(safe-area-inset-right)); }
  mark[data-type=bug]          { --pico-mark-background-color: var(--bone-mark-bug-bg, #c0392b1a); --pico-mark-color: var(--bone-mark-bug, #c0392b); }
  mark[data-type=announcement] { --pico-mark-background-color: var(--bone-mark-ann-bg, #1a7a8a1a); --pico-mark-color: var(--bone-mark-ann, #1a7a8a); }
  mark[data-type=request]      { --pico-mark-background-color: var(--bone-mark-req-bg, #b8860b1a); --pico-mark-color: var(--bone-mark-req, #b8860b); }
  mark[data-type=patch]        { --pico-mark-background-color: var(--bone-mark-patch-bg, #27ae601a); --pico-mark-color: var(--bone-mark-patch, #27ae60); }
  mark[data-type=release]      { --pico-mark-background-color: var(--bone-mark-rel-bg, #8e44ad1a); --pico-mark-color: var(--bone-mark-rel, #8e44ad); }
  mark[data-type=change]       { --pico-mark-background-color: var(--bone-mark-chg-bg, #2c3e501a); --pico-mark-color: var(--bone-mark-chg, #2c3e50); }
  mark { font-size: 0.75rem; font-weight: 600; text-transform: uppercase;
         letter-spacing: 0.05em; padding: 0.15rem 0.4rem; border-radius: 2px; }
  .toolbar { display: flex; gap: 0.75rem; flex-wrap: wrap; align-items: center; margin-bottom: 1rem; }
  .toolbar .create-new { margin-left: auto; padding: 0.3rem 0.7rem; font-size: 0.8rem; white-space: nowrap; }
  .filters { display: flex; gap: 0.4rem; flex-wrap: wrap; }
  .filters button { padding: 0.3rem 0.7rem; font-size: 0.8rem; }
  .filters button.outline { opacity: 0.5; }
  .filters.status-filters button.open-btn          { background: var(--bone-btn-open, var(--pico-secondary-background)); border-color: var(--bone-btn-open, var(--pico-secondary-border)); color: var(--bone-btn-open-text, var(--pico-secondary-inverse)); }
  .filters.status-filters button.open-btn.outline   { background: none; color: var(--bone-btn-open, var(--pico-secondary-background)); opacity: 0.5; }
  .filters.status-filters button.acked-btn         { background: var(--bone-btn-acked, var(--pico-primary-background)); border-color: var(--bone-btn-acked, var(--pico-primary-border)); color: var(--bone-btn-acked-text, var(--pico-primary-inverse)); }
  .filters.status-filters button.acked-btn.outline  { background: none; color: var(--bone-btn-acked, var(--pico-primary-background)); opacity: 0.5; }
  .filters.status-filters button.owned-btn         { background: var(--bone-btn-owned, var(--pico-contrast-background)); border-color: var(--bone-btn-owned, var(--pico-contrast-border)); color: var(--bone-btn-owned-text, var(--pico-contrast-inverse)); }
  .filters.status-filters button.owned-btn.outline  { background: none; color: var(--bone-btn-owned, var(--pico-contrast-background)); opacity: 0.5; }
  .filters.status-filters button.awaiting-btn         { background: var(--bone-btn-awaiting, #b8860b); border-color: var(--bone-btn-awaiting, #b8860b); color: var(--bone-btn-awaiting-text, #fff); }
  .filters.status-filters button.awaiting-btn.outline  { background: none; color: var(--bone-btn-awaiting, #b8860b); opacity: 0.5; }
  input[type=search] { max-width: 25vw; min-width: 200px; margin-bottom: 0; }
  th[data-sort] { cursor: pointer; user-select: none; white-space: nowrap; }
  th[data-sort]:hover { text-decoration: underline; }
  th[data-sort]::after { content: ' ↕'; opacity: 0.3; font-size: 0.75em; }
  th[data-sort].asc::after  { content: ' ↑'; opacity: 0.7; }
  th[data-sort].desc::after { content: ' ↓'; opacity: 0.7; }
  [data-theme=light] { --bone-stripe-bg: #f5f5f5; --bone-row-bg: #fff; }
  [data-theme=dark]  { --bone-stripe-bg: #1a1f2b; --bone-row-bg: #13171f; }
  tr.stripe td       { background-color: var(--bone-stripe-bg); }
  tr:not(.stripe) td { background-color: var(--bone-row-bg); }
  td:nth-child(3) { white-space: nowrap; }
  /* Fixed layout: deterministic across engines (the auto algorithm +
     percentage cells behave differently in Firefox and Chromium).
     Column widths come from the header row below; Subject, the only
     unsized column, absorbs ALL the leftover width.  Columns hidden by
     --html-columns or the responsive rules are display:none and their
     width goes back into the pool. */
  #reports-table table { width: 100%; table-layout: fixed; }
  #reports-table th:nth-child(1) { width: 110px; } /* Type */
  #reports-table th:nth-child(2) { width: 70px; }  /* Prio */
  #reports-table th:nth-child(3) { width: 100px; } /* Due */
  #reports-table th:nth-child(4) { width: 100px; } /* Flags */
  #reports-table th:nth-child(5) { width: 70px; }  /* Replies */
  #reports-table th:nth-child(6) { width: 160px; } /* Owner */
  #reports-table th:nth-child(7) { width: 160px; } /* Author */
  #reports-table th:nth-child(9) { width: 130px; } /* Date */
  td:nth-child(1) { white-space: nowrap; }                                            /* Type badge */
  td:nth-child(6) { white-space: nowrap; overflow: hidden; text-overflow: ellipsis; } /* Owner */
  td:nth-child(7) { white-space: nowrap; overflow: hidden; text-overflow: ellipsis; } /* Author */
  td:nth-child(9) { white-space: nowrap; }                                            /* Date */
  #status { font-size: 0.8rem; margin-bottom: 0.5rem; }
  .row-icon { font-size: 0.75rem; text-decoration: none; }
  a.row-icon[data-action] { margin-right: 0.3em; }
  /* Same footprint as the fold caret (char + margin), so unfolded
     children read as indented under their parent. */
  .child-indent { display: inline-block; width: 1.3em; }
  .vote-badge { display: inline-block; padding: 0.1rem 0.4rem; border-radius: 3px; font-size: 0.7rem;
                font-weight: 600; margin-left: 0.4em; margin-right: 0.4em; vertical-align: middle; }
  .vote-pos { background: var(--bone-vote-pos-bg, #27ae6033); color: var(--bone-vote-pos, #27ae60); }
  .vote-neg { background: var(--bone-vote-neg-bg, #c0392b33); color: var(--bone-vote-neg, #c0392b); }
  .vote-zero { background: var(--bone-vote-zero-bg, #95a5a622); color: var(--bone-vote-zero, #7f8c8d); }
  .theme-toggle { cursor: pointer; background: none; border: none; font-size: 1.2rem; padding: 0.3rem; }

  /* Responsive: progressively hide columns -- only Subject remains */
  @media (max-width: 1200px) {
    td:nth-child(4), th:nth-child(4) { display: none; } /* Flags */
  }
  @media (max-width: 1024px) {
    td:nth-child(3), th:nth-child(3) { display: none; } /* Due */
  }
  @media (max-width: 860px) {
    td:nth-child(2), th:nth-child(2) { display: none; } /* Priority */
  }
  @media (max-width: 780px) {
    td:nth-child(5), th:nth-child(5) { display: none; } /* Replies */
  }
  @media (max-width: 740px) {
    td:nth-child(9), th:nth-child(9) { display: none; } /* Date */
  }
  @media (max-width: 680px) {
    td:nth-child(6), th:nth-child(6) { display: none; } /* Owner */
  }
  @media (max-width: 540px) {
    td:nth-child(7), th:nth-child(7) { display: none; } /* Author */
    input[type=search] { max-width: none; min-width: 0; width: 100%; }
    .toolbar { flex-direction: column; align-items: stretch; }
  }
  @media (max-width: 420px) {
    td:nth-child(1), th:nth-child(1) { display: none; } /* Type */
  }
" footer-css))

;; ---------------------------------------------------------------------------
;; JS -- client-side filtering, sorting, URL state, theme toggle.
;; ---------------------------------------------------------------------------

(def ^:private index-js
  ;; The query engine (bone-search.js, node-testable, shared syntax
  ;; with gnaw.el) must load before the page logic that calls it.
  (str (slurp "resources/bone-search.js") "\n"
       (slurp "resources/bone-index.js")))

;; ---------------------------------------------------------------------------
;; Report-table columns (CLI: --html-columns / --html-columns-sort)
;; ---------------------------------------------------------------------------

;; Columns in their fixed render order.  --html-columns selects a subset;
;; unselected columns are hidden via CSS (nth-child position), the order is
;; not configurable.  Each pair maps the CLI name to the JS sort key used by
;; data-sort / boneConfig.columnsSort ("author" sorts on the "from" key).
(def ^:private canonical-columns
  [["type" "type"] ["priority" "priority"] ["due" "due"] ["flags" "flags"]
   ["replies" "replies"] ["owner" "owner"] ["author" "from"]
   ["subject" "subject"] ["date" "date"]])

(def ^:private column-names (mapv first canonical-columns))
(def ^:private column-sort-key (into {} canonical-columns))

(defn- parse-columns
  "Parse --html-columns CSV into an ordered vector of valid column names.
  Unknown names are dropped with a warning.  Blank/nil -> all columns."
  [s]
  (if (str/blank? s)
    column-names
    (let [known (set column-names)
          req   (->> (str/split s #",") (map str/trim) (remove str/blank?))
          bad   (remove known req)]
      (when (seq bad)
        (binding [*out* *err*]
          (println (str "Warning: unknown --html-columns ignored: " (str/join ", " bad)))))
      (or (not-empty (vec (distinct (filter known req)))) column-names))))

(defn- columns-mask-css
  "CSS hiding every column NOT in `selected`, by its 1-based nth-child
  position in the canonical order.  Returns nil when nothing to hide."
  [selected]
  (let [sel    (set selected)
        hidden (keep-indexed (fn [i nm] (when-not (sel nm) (inc i))) column-names)]
    (when (seq hidden)
      (str/join "\n"
                (map #(str "  td:nth-child(" % "), th:nth-child(" % ") { display: none; }")
                     hidden)))))

(defn- resolve-columns-sort
  "Map a --html-columns-sort CLI name to its JS sort key, or nil when the
  flag is absent -- the client then keeps the server's date-desc order.
  An unknown name warns and falls back to nil (default order)."
  [s]
  (when-not (str/blank? s)
    (let [nm (str/trim s)]
      (or (column-sort-key nm)
          (binding [*out* *err*]
            (println (str "Warning: unknown --html-columns-sort '" nm "', using default order"))
            nil)))))

;; The page is a static shell: open reports are no longer inlined as
;; `boneData`.  The client fetches them from `openJsonUrl` at load time
;; (mirroring how closed reports are lazy-loaded from `closedJsonUrl`),
;; so index.html stays byte-identical across data-only changes.
(defn page-js [source-type page-size columns-sort]
  (wrap-js (str "var boneConfig = {typeLabels:" (escape-script-payload (json/generate-string type-labels))
                ",openJsonUrl:'reports/all-open.json'"
                ",closedJsonUrl:'reports/all-closed.json'"
                (when source-type
                  (str ",sourceType:'" source-type "'"))
                (when page-size
                  (str ",pageSize:" page-size))
                (when columns-sort
                  (str ",columnsSort:'" columns-sort "'"))
                "};\n"
                index-js "\n"
                theme-toggle-js)))

;; ---------------------------------------------------------------------------
;; Page assembly
;; ---------------------------------------------------------------------------

;; The shell embeds no report data: only stable metadata (source
;; name/type, RSS/ICS presence) shapes the markup, so the file does not
;; change when reports do.
(defn index-page [reports-dir envelope page-size columns columns-sort]
  (let [mask-css     (columns-mask-css columns)
        source-type  (get envelope "source-type")
        source-name  (get envelope "source")
        website      (get envelope "website")
        contribute   (get envelope "contribute-url")
        title        (page-title "Reports" source-name)
        has-rss?     (.exists (clojure.java.io/file reports-dir "all.xml"))
        ;; ICS lives one level up from reports-dir, in events/
        base-dir     (.getParent (clojure.java.io/file reports-dir))
        has-ical?    (and base-dir (.exists (clojure.java.io/file base-dir "events" "announcements.ics")))
        rss-href     "reports/all.xml"
        ;; Sortable column headers: focusable (tabindex) with an Enter/
        ;; Space handler (thSortKey in bone-index.js) so keyboard users
        ;; can sort; aria-sort is maintained by the JS.  The ↩ glyph
        ;; gets an :aria-label so screen readers hear "Replies".
        cols         (mapv (fn [[k i label tip aria]]
                             [:th (cond-> {:data-sort k
                                           :scope     "col"
                                           :tabindex  "0"
                                           :onclick   (str "sortTable(" i ",'" k "')")
                                           :onkeydown "thSortKey(event)"
                                           :title     tip}
                                    aria (assoc :aria-label aria))
                              label])
                           [["type"     0 "Type"    "Sort by type"              nil]
                            ["priority" 1 "Prio"    "Sort by priority"          "Priority"]
                            ["due"      2 "Due"     "Sort by deadline"          nil]
                            ["flags"    3 "Flags"   "Sort by flags"             nil]
                            ["replies"  4 "↩"       "Sort by number of replies" "Replies"]
                            ["owner"    5 "Owner"   "Sort by owner"             nil]
                            ["from"     6 "Author"  "Sort by author"            nil]
                            ["subject"  7 "Subject" "Sort by last activity"     nil]
                            ["date"     8 "Date"    "Sort by date"              nil]])
        tpl-body     (str
                      (h/html
                       [:main.container
                        (nav-bar (page-title "Reports" nil) "reports"
                                 {:source source-name :source-href website})
                        [:p.generated-at
                         {:id "generated-at"
                          :style "font-size:0.78rem;color:var(--pico-muted-color);margin-bottom:1rem"}]
                        [:div.toolbar
                         [:input#si {:type        "search"
                                     :aria-label  "Search reports"
                                     :placeholder "Press / to search -- see Docs for syntax"
                                     :oninput     "onSearchInput()"}]
                         ;; Filled client-side from the fetched reports
                         ;; (buildTypeFilters), so the shell stays data-independent.
                         [:div.filters {:id "type-filters"}]
                         [:div.filters.status-filters
                          [:button#btn-open.open-btn
                           {:onclick "toggleOpen(this)" :title "Toggle visibility of open reports only"
                            :aria-pressed "true"}
                           "Open"]
                          [:button#btn-acked.acked-btn.outline
                           {:onclick "toggleAcked(this)" :title "Toggle visibility of acked reports"
                            :aria-pressed "false"}
                           "Acked"]
                          [:button#btn-owned.owned-btn.outline
                           {:onclick "toggleOwned(this)" :title "Toggle visibility of owned reports"
                            :aria-pressed "false"}
                           "Owned"]
                          [:button#btn-awaiting.awaiting-btn.outline
                           {:onclick "toggleAwaiting(this)" :title "Show only reports awaiting reply"
                            :aria-pressed "false"
                            :style "margin-left:auto"}
                           [:span {:aria-hidden "true"} "⌚ "] "Awaiting"]]
                         (when contribute
                           [:a.create-new {:href contribute :role "button"
                                           :title "Create a new report"}
                            "Create new"])]
                        [:div#status {:role "status"}]
                        [:figure {:id "reports-table" :style "overflow-x:auto"}
                         [:table
                          [:thead [:tr (seq cols)]]
                          [:tbody {:id "data"}]]]
                        [:div#empty-state {:style "display:none;margin:1.5rem 0"}
                         [:p {:style "margin-bottom:0.5rem"} "No matching report.  A few suggestions:"]
                         [:ul {:style "margin:0"}
                          [:li [:a {:href "?types=bug&acked=1&sort=date&dir=asc"} "Fix confirmed bugs"]]
                          [:li [:a {:href "?types=patch&sort=date&dir=asc"} "Review old patches"]]
                          [:li [:a {:href "?types=request&sort=replies&dir=desc"} "Answer active requests"]]
                          [:li [:a {:href "?acked=1&awaiting=1&sort=date&dir=asc"} "Revive reports awaiting reporter input"]]]]
                        [:div#pagination]])
                      (h/html (bone-footer {:ical has-ical?
                                            :website website
                                            :source source-name})))]
    (str
     "<!DOCTYPE html>\n"
     (h/html
      [:html {:lang "en" :data-theme "light"}
       [:head
        [:meta {:charset "UTF-8"}]
        [:meta {:name "viewport" :content "width=device-width, initial-scale=1"}]
        [:meta {:name "color-scheme" :content "light dark"}]
        [:meta {:name "description" :content bone-description}]
        [:meta {:property "og:title" :content title}]
        [:meta {:property "og:description" :content bone-description}]
        [:meta {:property "og:type" :content "website"}]
        [:link {:rel "stylesheet" :href pico-cdn}]
        (for [{:keys [link inline]} (resolved-theme)]
          (if link
            [:link {:rel "stylesheet" :href link}]
            [:style (h/raw inline)]))
        (when has-rss?
          [:link {:rel "alternate" :type "application/rss+xml"
                  :title "BONE Reports RSS" :href rss-href}])
        [:title title]
        [:style (h/raw page-css)]
        (when mask-css [:style (h/raw mask-css)])]
       [:body
        (noscript-banner title)
        (h/raw (wrap-template "js-tpl" tpl-body))
        [:script (h/raw (page-js source-type page-size columns-sort))]]]))))

;; ---------------------------------------------------------------------------
;; Main
;; ---------------------------------------------------------------------------

(let [{:keys [out-file json-file out-dir theme page-size html-columns html-columns-sort]}
      (parse-cli-args *command-line-args*)
      _            (when theme (set-theme! theme))
      columns      (parse-columns html-columns)
      columns-sort (resolve-columns-sort html-columns-sort)]
  (when (or (str/blank? json-file) (str/blank? out-file))
    (binding [*out* *err*]
      (log/error "bone-index.clj requires --json <file> and -o <file>"))
    (System/exit 2))
  (let [;; getParent is nil for a bare filename: default to ".".
        reports-dir (or out-dir (.getParent (clojure.java.io/file json-file)) ".")]
    (io/make-parents out-file)
    (let [envelope (json/parse-string (slurp json-file))
          n-open   (count (get envelope "reports"))
          html     (index-page reports-dir envelope page-size columns columns-sort)]
      (spit-html out-file html)
      ;; Log to stdout (not stderr): this is routine progress, captured in
      ;; the export log, not a cron-mail trigger.  Real errors still go to
      ;; stderr above.
      (log/info "Wrote shell" out-file "(" n-open "open reports in JSON)"))))
