;; Copyright (c) 2026 Bastien Guerry <bzg@gnu.org>
;; SPDX-License-Identifier: EPL-2.0
;; License-Filename: LICENSES/EPL-2.0.txt

(ns bone.config
  "config.edn loading (reader tags, :password-file resolution), the
  derived per-source map, mailmap.edn and the DB path.  No datalevin
  dependency: shared by the JVM daemon and the bb scripts."
  (:require [bone.common :as common]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.walk :as walk]
            [taoensso.timbre :as log]))

;; ---------------------------------------------------------------------------
;; Config and source-map
;; ---------------------------------------------------------------------------

(def ^:private config-edn-readers
  "EDN reader tags accepted in `config.edn`.

   `#bone/env \"VAR\"` resolves to the named environment variable.
   Raises if the variable is unset -- prefer a clear startup failure
   over silently using a nil password."
  {'bone/env (fn [var-name]
               (let [k (str var-name)]
                 (or (System/getenv k)
                     (throw (ex-info (str "Environment variable not set: " k)
                                     {:var k})))))})

(defn- resolve-password-files
  "Replace every `{:password-file path}` in the config with the trimmed
   contents of the file.  Applied across :mailboxes and :notifications
   :smtp.  Raises if both `:password` and `:password-file` are set, or
   if the referenced file is missing."
  [config]
  (walk/postwalk
   (fn [x]
     (if-let [pf (and (map? x) (:password-file x))]
       (do
         (when (:password x)
           (throw (ex-info ":password and :password-file are mutually exclusive"
                           {:offending x})))
         (let [f (io/file (common/expand-home pf))]
           (when-not (.exists f)
             (throw (ex-info (str ":password-file not found: " pf)
                             {:path pf})))
           (-> x (dissoc :password-file)
               (assoc :password (str/trim (slurp f))))))
       x))
   config))

(defn load-config
  "Load config.edn if it exists, or nil.  With no args, consults the
  BONE_CONFIG env var, falling back to ./config.edn -- so all bb
  scripts honor a single override point without per-script flags.

  Resolves `#bone/env \"VAR\"` reader tags and `:password-file` entries
  before returning, so callers see a fully-materialised config."
  ([] (load-config (or (System/getenv "BONE_CONFIG") "config.edn")))
  ([path]
   (let [f (io/file path)]
     (when (.exists f)
       (-> (edn/read-string {:readers config-edn-readers} (slurp f))
           resolve-password-files)))))

(defn inline-password-locations
  "Read config.edn as it was written -- without resolving #bone/env or
   :password-file -- and return a vector of human-readable location
   labels where :password is a plain inline string.  Used by
   `bb test-config` to remind operators that credentials can live in
   a sidecar file or environment variable instead."
  [path]
  (let [f (io/file path)]
    (when (.exists f)
      (let [env-marker ::env
            raw        (edn/read-string
                        {:readers {'bone/env (constantly env-marker)}}
                        (slurp f))
            inline?    #(and (map? %)
                             (string? (:password %))
                             (not (:password-file %)))
            mailboxes  (for [mb (:mailboxes raw) :when (inline? mb)]
                         (str "mailbox " (pr-str (:name mb))))
            smtp       (when (inline? (get-in raw [:notifications :smtp]))
                         "notifications :smtp")]
        (vec (cond-> (vec mailboxes) smtp (conj smtp)))))))

(defn load-mailmap
  "Load ./mailmap.edn (shape `{\"Canonical Name\" [emails...]}`) and return
  `{email-lc -> canonical-name}` (inverted, flat).  Returns `{}` if
  the file is absent.  Export-only -- never read on the ingest path."
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

(defn db-path
  "Resolve the Datalevin DB path: prefer :db {:path ...} from the
  config, fall back to BONE_DB env var, then default \"data/bone-db\"."
  [config]
  (or (get-in config [:db :path])
      (System/getenv "BONE_DB")
      "data/bone-db"))

(defn build-source-map
  "Build source-name -> config map from config."
  [config]
  (let [global-st       (:labels config)
        global-cmd      (:commands config)
        global-ef       (:export-formats config)
        global-expiry   (:expiry config)
        global-rt       (:report-types config)
        global-cs       (:command-syntax config)
        global-restr    (:restricted-types config)
        global-ptrig?   (contains? config :patch-triggers?)
        global-ptrig    (:patch-triggers? config)]
    (into {}
          (keep (fn [src]
                 (if-let [stype (common/source-type src)]
                   (let [restricted (if (contains? src :restricted-types)
                                      (:restricted-types src)
                                      global-restr)
                         ptrig-set? (or (contains? src :patch-triggers?)
                                        global-ptrig?)
                         ptrig      (if (contains? src :patch-triggers?)
                                      (:patch-triggers? src)
                                      global-ptrig)]
                     [(:name src)
                      (merge {:source-type stype}
                             (select-keys src [:list :alias :to :commands :labels :notifications
                                               :archive-format-string :list-archive :base-url
                                               :website :contribute-url :post-address
                                               :maintainers :awaiting-delay :periods])
                             (when global-st {:global-labels global-st})
                             (when global-cmd {:global-commands global-cmd})
                             (cond-> {:export-formats (set (or (:export-formats src) global-ef ["json" "org" "rss"]))
                                      :report-types (when-let [rt (or (:report-types src) global-rt)]
                                                      (set (map keyword rt)))
                                      :expiry (or (:expiry src) global-expiry)
                                      :command-syntax (or (:command-syntax src) global-cs :loose)}
                               (some? restricted)
                               (assoc :restricted-types (set (map keyword restricted)))

                               ptrig-set?
                               (assoc :patch-triggers? (boolean ptrig))))])
                   (log/warn "Source has no :list, :alias, or :to key -- skipping:" (:name src)))))
          (:sources config))))
