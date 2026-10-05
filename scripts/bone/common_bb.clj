;; Copyright (c) 2026 Bastien Guerry <bzg@gnu.org>
;; SPDX-License-Identifier: EPL-2.0
;; License-Filename: LICENSES/EPL-2.0.txt

(ns bone.common-bb
  "Babashka-only extensions to `bone.common`: Datalevin pod bindings
  and DB queries that run via the pod.  Scripts require this ns
  alongside `bone.common`; there is no more `load-file` dance."
  (:require [bone.common :as common]
            [taoensso.timbre :as log]))

(log/merge-config! {:min-level :info})

;; ---------------------------------------------------------------------------
;; Datalevin pod (bb-only -- JVM uses datalevin.core directly)
;; ---------------------------------------------------------------------------

(def ^:private datalevin-version "1.1.0")

(defn load-datalevin-pod! []
  (require '[babashka.pods :as pods])
  ((resolve 'pods/load-pod) 'huahaiy/datalevin datalevin-version)
  (require '[pod.huahaiy.datalevin :as d]))

(def ^:private d-q    (delay (resolve 'pod.huahaiy.datalevin/q)))
(def ^:private d-pull (delay (resolve 'pod.huahaiy.datalevin/pull)))
(defn dq   "Resolved d/q"    [& args] (apply @d-q args))
(defn dpull "Resolved d/pull" [& args] (apply @d-pull args))

;; ---------------------------------------------------------------------------
;; Datalevin queries (bb-only -- JVM equivalents are inline)
;; ---------------------------------------------------------------------------

(defn all-reports
  "Fetch all reports. Must be called after load-datalevin-pod!."
  [db]
  (->> (dq (list :find (list 'pull '?r common/report-pull-pattern)
                 :where ['?r :report/type '_])
           db)
       (map first)))

(defn fetch-attachment-data
  "Fetch attachment data for a single report by message-id.
  Returns the attachment-pull-pattern projection, or nil."
  [db message-id]
  (when message-id
    (dpull db common/attachment-pull-pattern
           [:report/message-id-hash (common/mid-hash message-id)])))

(defn get-tenures
  "Fetch all maintainer tenures (active and closed) for `source-name`.
  Returns a vector of tenure maps {:eid :email :from :to :order}."
  [db source-name]
  (let [eids (dq '[:find [?e ...]
                   :in $ ?src
                   :where [?e :maint-tenure/source ?src]]
                 db source-name)]
    (mapv #(common/tenure-map (dpull db common/tenure-pull-pattern %))
          eids)))

(defn tenures-snapshot
  "Return a serialization-friendly view of all tenures for a source,
  suitable for embedding in JSON/EDN exports. Each entry has:
    :email  lower-cased address
    :from   ISO date string \"yyyy-MM-dd\" or nil (= since the beginning)
    :to     ISO date string or nil (= currently active)
    :order  integer index from config.edn (absent when unknown)
    :lead?  true iff this is the currently-active lead tenure
  The list is sorted active-first (by :from asc, nil first), then closed
  tenures by :to desc -- matching how the HTML docs render them."
  [tenures]
  (let [lead   (common/lead-maintainer tenures)
        sort-k (fn [{:keys [from to]}]
                 [(if to 1 0)
                  (if to
                    (- (.getTime ^java.util.Date to))
                    (if-let [^java.util.Date f from] (.getTime f) 0))])]
    (->> tenures
         (sort-by sort-k)
         (mapv (fn [{:keys [email from to order]}]
                 (cond-> {:email email
                          :from  (common/format-date-iso from)
                          :to    (common/format-date-iso to)
                          :lead? (and (nil? to) (= email lead))}
                   order (assoc :order order)))))))

(defn get-last-modified [db]
  (dq '[:find ?t .
        :where [?e :meta/ident "global"] [?e :meta/last-modified ?t]]
      db))

(defn- source-types-since
  "Return a map {source-name {report-type count}} for reports whose
  `attr` timestamp is after `since-ts`.  A present type maps to a
  truthy count, so the map doubles as a set-like predicate."
  [db attr since-ts]
  (reduce (fn [m [src rtype n]]
            (assoc-in m [src rtype] n))
          {}
          (dq '[:find ?src ?t (count ?r)
                :in $ ?a ?since
                :where
                [?r ?a ?u] [(> ?u ?since)]
                [?r :report/type ?t]
                [?r :report/email ?e] [?e :email/source ?src]]
              db attr since-ts)))

(defn changed-source-types-since
  "Reports *touched* after `since-ts` (`:report/updated-at`): status
  changes, new replies, relations...  Drives the source-level and
  per-type re-export skip logic."
  [db since-ts]
  (source-types-since db :report/updated-at since-ts))

(defn new-source-types-since
  "Reports *created* (ingested) after `since-ts` (`:report/created-at`
  -- the wall-clock ingestion instant, not the email's Date header,
  which may be back- or future-dated).  Counts only genuine additions,
  so the cron notification can tell new reports apart from updates."
  [db since-ts]
  (source-types-since db :report/created-at since-ts))

(defn state-changed-source-types-since
  "Reports whose own *state* changed after `since-ts`
  (`:report/state-changed-at`), excluding mere thread growth.  Used by
  the cron notification to count effective report modifications."
  [db since-ts]
  (source-types-since db :report/state-changed-at since-ts))
