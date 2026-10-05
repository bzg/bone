;; Copyright (c) 2026 Bastien Guerry <bzg@gnu.org>
;; SPDX-License-Identifier: EPL-2.0
;; License-Filename: LICENSES/EPL-2.0.txt

(ns bone.html-bb
  "Shared HTML utilities for Babashka scripts: CDN theme resolution,
  <head> builder, nav bar, org->HTML helpers, and an optional
  `tidy` pretty-printer."
  (:require [babashka.process]
            [clojure.string :as str]
            [hiccup2.core :as h]))

;; ---------------------------------------------------------------------------
;; Shared CDN
;; ---------------------------------------------------------------------------

(def pico-cdn "https://cdn.jsdelivr.net/npm/@picocss/pico@2/css/pico.min.css")

;; ---------------------------------------------------------------------------
;; Theme CDN -- set via config.edn :theme or --html-theme CLI flag
;; ---------------------------------------------------------------------------

(def ^:private themes-cdn
  "Base URL for bzg/pico-themes on jsDelivr."
  "https://cdn.jsdelivr.net/gh/bzg/pico-themes@latest/")

(defn- resolve-css-theme
  "Resolve a --html-theme / :theme value to a seq of maps, or nil.
  Each map is either {:link url} or {:inline css-content}.

  Resolution order:
  1. https:// URL           => [{:link url}]
  2. file:///path           => [{:inline (slurp path)}]
  3. path ending in .css    => [{:inline (slurp path)}] (relative or absolute)
  4. bare name (no spaces)  => pico-themes CDN [{:link base} {:link bone-overlay}]
  5. \"none\" or nil          => nil"
  [theme]
  (when (and theme (not= theme "none"))
    (cond
      (str/starts-with? theme "https://")
      [{:link theme}]

      (str/starts-with? theme "file:///")
      (let [path (subs theme (count "file://"))]
        [{:inline (slurp path)}])

      (str/ends-with? theme ".css")
      (let [f (java.io.File. theme)]
        (if (.isFile f)
          [{:inline (slurp f)}]
          (do (binding [*out* *err*]
                (println (str "Warning: CSS file not found: " theme)))
              nil)))

      (not (str/includes? theme " "))
      [{:link (str themes-cdn theme ".css")}
       {:link (str themes-cdn "bone/" theme ".css")}]

      :else
      (do (binding [*out* *err*]
            (println (str "Warning: cannot resolve theme '" theme "'")))
          nil))))

(def ^:private theme-atom (atom nil))

(defn set-theme!
  "Set the resolved theme from a name, path, or URL. Called by each script at startup."
  [theme]
  (reset! theme-atom (resolve-css-theme theme)))

(defn resolved-theme
  "Current resolved theme entries as a seq of maps, or nil."
  []
  @theme-atom)

;; ---------------------------------------------------------------------------
;; Shared metadata
;; ---------------------------------------------------------------------------

(def bone-description "BONE (Bug And Report Keeper) -- track bugs, patches, and requests.")
(def ^:private bone-repo-url "https://codeberg.org/bzg/bone")

;; ---------------------------------------------------------------------------
;; LibreJS license tags (JS files are MPL-2.0)
;; ---------------------------------------------------------------------------

(def ^:private js-license-start
  "// @license magnet:?xt=urn:btih:3877d6d54b3accd4bc32f8a48bf32ebc0901502a&dn=mpl-2.0.txt MPL-2.0")
(def ^:private js-license-end
  "// @license-end")

(defn wrap-js
  "Wrap JavaScript code with LibreJS license tags."
  [js]
  (str js-license-start "\n" js "\n" js-license-end))

;; ---------------------------------------------------------------------------
;; Shared JS (loaded from resources/ at build time)
;; ---------------------------------------------------------------------------

(def theme-toggle-js (slurp "resources/bone-theme.js"))

;; ---------------------------------------------------------------------------
;; Shared <head> builder
;; ---------------------------------------------------------------------------

(defn html-escape
  "Escape HTML special characters in a string (element and attribute
  contexts)."
  [s]
  (when s
    (-> s
        (str/replace "&" "&amp;")
        (str/replace "<" "&lt;")
        (str/replace ">" "&gt;")
        (str/replace "\"" "&quot;"))))

(defn head-hiccup
  "The shared <head> block as a hiccup vector.
   opts keys:
     :title    -- page <title> (required)
     :css      -- inline CSS string (optional)
     :rss-href -- href for <link rel=alternate> RSS (optional)
     :extra    -- hiccup appended before </head> (optional)"
  [{:keys [title css rss-href extra]}]
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
   (when rss-href
     [:link {:rel "alternate" :type "application/rss+xml"
             :title "BONE Reports RSS" :href rss-href}])
   [:title title]
   (when css [:style (h/raw css)])
   extra])

(defn html-head
  "Render a <head> block as a string, for the pages assembled from
  strings (see `head-hiccup` for the hiccup ones).
   opts keys:
     :title      -- page <title> (required)
     :css        -- inline CSS string (optional)
     :extra-head -- raw HTML string inserted before </head> (optional)"
  [{:keys [title css extra-head]}]
  (let [title (html-escape title)]
    (str "<head>\n"
         "<meta charset=\"UTF-8\">\n"
         "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">\n"
         "<meta name=\"color-scheme\" content=\"light dark\">\n"
         "<meta name=\"description\" content=\"" bone-description "\">\n"
         "<meta property=\"og:title\" content=\"" title "\">\n"
         "<meta property=\"og:description\" content=\"" bone-description "\">\n"
         "<meta property=\"og:type\" content=\"website\">\n"
         "<link rel=\"stylesheet\" href=\"" pico-cdn "\">\n"
         (when-let [entries (resolved-theme)]
           (str/join (map (fn [{:keys [link inline]}]
                            (if link
                              (str "<link rel=\"stylesheet\" href=\"" link "\">\n")
                              (str "<style>\n" inline "\n</style>\n")))
                          entries)))
         "<title>" title "</title>\n"
         (when css (str "<style>\n" css "\n</style>\n"))
         (or extra-head "")
         "</head>\n")))

(def theme-toggle-css
  ".theme-toggle { cursor: pointer; background: none; border: none; font-size: 1.2rem; padding: 0.3rem; }")

;; ---------------------------------------------------------------------------
;; Shared nav theme-toggle button (hiccup vector)
;; ---------------------------------------------------------------------------

(defn- theme-toggle-btn []
  [:button.theme-toggle
   {:onclick "toggleTheme()" :aria-label "Toggle theme"}
   [:span#theme-icon "🌙"]])

;; ---------------------------------------------------------------------------
;; Shared nav bar (hiccup vector)
;; ---------------------------------------------------------------------------

(def ^:private nav-pages
  "Navigation pages in display order: [id label href]."
  [["reports" "Reports" "index.html"]
   ["docs"   "Docs"  "docs.html"]
   ["data"    "Data"    "data.html"]])

(defn nav-bar
  "Render a <nav> with title and Reports/Docs/Data links.
  `current` is the id of the active page (bolded).  When `current` is
  nil, the per-source page links are omitted -- useful on the root
  index where those paths live one directory deeper.  Options:
    :source       source name appended to the title (\" - <source>\")
    :source-href  when set, the source name links there (the project
                  website) instead of staying plain text."
  ([title current] (nav-bar title current nil))
  ([title current {:keys [source source-href]}]
   [:nav
    [:ul [:li [:a {:href "index.html" :title "Back to BONE homepage"
                   :style "text-decoration:none;color:inherit"}
               [:strong title]]
          (when source
            (list " - "
                  (if source-href
                    [:a {:href source-href
                         :title (str "Visit " source " website")
                         :style "text-decoration:none;color:inherit"}
                     [:strong source]]
                    [:strong source])))]]
    [:ul
     (when current
       (for [[id label href] nav-pages]
         [:li (if (= id current)
                [:a {:href href} [:strong label]]
                [:a {:href href} label])]))
     [:li (theme-toggle-btn)]]]))

;; ---------------------------------------------------------------------------
;; Shared page title
;; ---------------------------------------------------------------------------

(defn page-title
  "Build a 'BONE - <kind>' page title, appending the source name when set."
  [kind source-name]
  (str "BONE - " kind (when source-name (str " - " source-name))))

;; ---------------------------------------------------------------------------
;; JS-less fallback: <noscript> banner + script-template injection
;; ---------------------------------------------------------------------------

(defn noscript-banner
  "Render a <noscript> block with a title and a flat list of entry
  points for JavaScript-less browsers (eww, w3m, Lynx).  This is the
  entire visible content of the page in those browsers."
  [title]
  [:noscript
   [:h1 title]
   [:ul
    [:li [:a {:href "data.html"} "Available data"] " -- JSON, RSS, Org, iCal files"]
    [:li [:a {:href "docs.html"} "Documentation"]]
    [:li [:a {:href "reports/all.xml"} "Subscribe via RSS"]]
    [:li [:a {:href "https://codeberg.org/bzg/gnaw"} "gnaw"] " -- CLI based on fzf"]
    [:li [:a {:href "https://codeberg.org/bzg/gnaw.el"} "gnaw.el"] " -- GNU Emacs client"]
    [:li [:a {:href bone-repo-url} "BONE source repository"]]]])

(defn wrap-template
  "Embed raw HTML inside a non-executable <script type=\"text/x-html\">
  and emit a bootstrap that re-injects it via document.write.  JS-less
  browsers (eww, w3m, lynx) ignore the script body entirely; browsers
  with JS run the bootstrap during parsing and the content lands in the
  DOM at that position.

  A literal </script> in `body` would close the outer template, so it
  is replaced with a placeholder and restored client-side.  The match
  is case-insensitive, like the HTML parser's end-tag scan."
  [id body]
  (str "<script type=\"text/x-html\" id=\"" id "\">"
       (str/replace body #"(?i)</script>" "__BONE_END_SCRIPT__")
       "</script>\n"
       "<script>document.write("
       "document.getElementById('" id "').textContent"
       ".replace(/__BONE_END_SCRIPT__/g,'<\\/script>'));</script>\n"))

;; ---------------------------------------------------------------------------
;; Shared footer (hiccup vector)
;; ---------------------------------------------------------------------------

(def footer-css
  "footer.bone-footer { font-size:0.78rem; color:var(--pico-muted-color); text-align:center; padding:2rem 0 1rem; }")

(defn bone-footer
  "Footer with BONE repo + license link.
  Options:
    :feeds    when true (default), appends per-source RSS/JSON/Org links.
              Set to false on the root index where those paths don't exist.
    :ical     when true (default) and :feeds is on, appends an iCal link.
    :website  when set, appends a link to the tracked project's website,
              labeled with :source (the source name) when available."
  [{:keys [ical feeds website source] :or {ical true feeds true}}]
  [:footer.bone-footer
   [:a {:href bone-repo-url} "BONE"]
   " is "
   [:a {:href "https://www.gnu.org/philosophy/free-sw.html"}
    "Free Software"]
   (when feeds
     (list " -- "
           [:a {:href "reports/all.xml"} "RSS"]
           " -- "
           [:a {:href "reports/all.json"} "JSON"]
           " -- "
           [:a {:href "reports/all.org"} "Org"]
           (when ical
             (list " -- " [:a {:href "events/announcements.ics"} "iCal"]))))
   (when website
     (list " -- " [:a {:href website} (or source "Website")]))])

;; ---------------------------------------------------------------------------
;; Org-mode inline link conversion (shared by bone-docs, bone-stats)
;; ---------------------------------------------------------------------------

(defn org-inline-links
  "Convert org-mode links to HTML anchors.
  [[url][label]] -> <a href=\"url\">label</a>
  [[url]]        -> <a href=\"url\">url</a>"
  [s]
  ;; Quotes are not covered by the caller's & < > escaping; escape them
  ;; in the href so a URL cannot break out of the attribute.
  (let [href #(str/replace % "\"" "&quot;")]
    (-> s
        (str/replace #"\[\[([^\]]+)\]\[([^\]]+)\]\]"
                     (fn [[_ url label]]
                       (str "<a href=\"" (href url) "\">" label "</a>")))
        (str/replace #"\[\[([^\]]+)\]\]"
                     (fn [[_ url]]
                       (str "<a href=\"" (href url) "\">" url "</a>"))))))

(defn org-inline
  "Convert a single line of inline org markup to HTML, with HTML
  escaping applied first so `&`, `<`, `>` in user text cannot break
  the output.  Supports [[links]], =code=, *bold*, /italic/ and \\vert."
  [s]
  (-> s
      (str/replace "&" "&amp;")
      (str/replace "<" "&lt;")
      (str/replace ">" "&gt;")
      org-inline-links
      (str/replace #"=([^=\n\"]+)=" "<code>$1</code>")
      (str/replace #"(?<=\s|^)\*([^*\n]+)\*(?=[\s.,;:!?)]|$)" "<strong>$1</strong>")
      (str/replace #"(?<=\s|^)/([^/\n]+)/(?=[\s.,;:!?)]|$)" "<em>$1</em>")
      (str/replace "\\vert" "|")))

(defn parse-org-table
  "Parse a seq of org-table lines (as strings) into an HTML table.
  Hlines (|-----+---|) are dropped; the first surviving row becomes
  <thead>, the rest <tbody>. Each cell is passed through org-inline
  so links, =code=, *bold* are rendered and HTML characters escaped."
  [lines]
  (let [rows (->> lines
                  (remove #(re-matches #"\s*\|[-+]+\|\s*" %))
                  (mapv (fn [line]
                          (->> (str/split line #"\|" -1)
                               (drop 1) butlast
                               (mapv str/trim)))))]
    (when (seq rows)
      (let [header (first rows)
            body   (rest rows)]
        (str "<table>\n<thead><tr>"
             (str/join (map #(str "<th>" (org-inline %) "</th>") header))
             "</tr></thead>\n<tbody>\n"
             (str/join (map (fn [r]
                              (str "<tr>"
                                   (str/join (map #(str "<td>" (org-inline %) "</td>") r))
                                   "</tr>\n"))
                            body))
             "</tbody></table>")))))

;; ---------------------------------------------------------------------------
;; HTML tidy (optional -- graceful no-op when tidy is not installed)
;; ---------------------------------------------------------------------------

(def ^:private tidy-available?
  "True when the `tidy` binary is on PATH."
  (delay
    (try
      (-> (babashka.process/process ["tidy" "--version"]
                                    {:out :string :err :string})
          deref :exit (= 0))
      (catch Exception _ false))))

(defn- tidy-html
  "Pretty-print an HTML string via tidy.  Returns the input unchanged if
  tidy is not installed or if it fails on the input."
  [html]
  (if-not @tidy-available?
    html
    (try
      (let [result @(babashka.process/process
                     ["tidy" "-qi" "--wrap" "0" "--tidy-mark" "no"]
                     {:in html :out :string :err :string})]
        ;; tidy exits 0 = ok, 1 = warnings (normal), 2 = errors
        (if (<= (:exit result) 1)
          (:out result)
          html))
      (catch Exception _ html))))

(defn spit-html
  "Write `html` to `file`, running it through tidy when available."
  [file html]
  (spit file (tidy-html html)))
