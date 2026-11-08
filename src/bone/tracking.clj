;; Copyright (c) 2026 Bastien Guerry <bzg@gnu.org>
;; SPDX-License-Identifier: EPL-2.0
;; License-Filename: LICENSES/EPL-2.0.txt

(ns bone.tracking
  "Change-tracking helpers for incremental export.
  Separated from bone.common so that common stays datalevin-free."
  (:require [datalevin.core :as d])
  (:import [java.util Date]))

(def ^:const meta-ident "global")

(defn bump-report-updated!
  "Mark reports (eid or coll) as changed for incremental export:
  sets :report/updated-at and :meta/last-modified to now.  When
  `state-change?` (default true), also sets :report/state-changed-at,
  the last *effective* change to the report itself; pass false for
  contextual changes (new reply, own creation) that must re-export
  but are not state modifications for the cron notification."
  ([conn report-eid] (bump-report-updated! conn report-eid true))
  ([conn report-eid state-change?]
   (let [now  (Date.)
         eids (if (coll? report-eid) report-eid [report-eid])
         base (cond-> {:report/updated-at now}
                state-change? (assoc :report/state-changed-at now))]
     (d/transact! conn (into [{:meta/ident meta-ident :meta/last-modified now}]
                             (map (fn [eid] (assoc base :db/id eid)))
                             eids)))))

(defn bump-global-modified!
  "Bump :meta/last-modified without targeting a specific report."
  [conn]
  (d/transact! conn [{:meta/ident meta-ident :meta/last-modified (Date.)}]))

(defn ensure-created-at!
  "Stamp :meta/created-at once, the first time the database is opened:
  its birth.  bb notify mails no command failure whose email predates
  it, so a --fresh replay stays silent."
  [conn]
  (let [db (d/db conn)]
    (when-not (some->> (d/entid db [:meta/ident meta-ident])
                       (d/pull db [:meta/created-at])
                       :meta/created-at)
      (d/transact! conn [{:meta/ident meta-ident :meta/created-at (Date.)}]))))
