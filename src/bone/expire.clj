;; Copyright (c) 2026 Bastien Guerry <bzg@gnu.org>
;; SPDX-License-Identifier: EPL-2.0
;; License-Filename: LICENSES/EPL-2.0.txt

(ns bone.expire
  "Close open reports when their age and state match the configured
  :expiry rules. Runs inside the daemon."
  (:require [clojure.string :as str]
            [datalevin.core :as d]
            [taoensso.timbre :as log]
            [bone.common :as common]
            [bone.lookup :as lookup]
            [bone.tracking :as tracking])
  (:import [java.util Date]))

;; ---------------------------------------------------------------------------
;; Rule evaluation
;; ---------------------------------------------------------------------------

(defn- parse-delay-safe
  "Like common/parse-delay but logs and returns nil on an invalid
  duration (parse-delay throws on unknown units): a bad :expiry rule
  must skip the rule, not kill the daemon."
  [v]
  (try (common/parse-delay v)
       (catch Exception e
         (log/warn "Ignoring expiry rule with invalid duration:"
                   (pr-str v) "--" (ex-message e))
         nil)))

(defn- parse-expiry-rule
  "Normalize an expiry rule map.
  Expects a map with :inactive-after (integer, duration string, ISO date, or :deadline)."
  [v]
  (when (map? v)
    (let [after (:inactive-after v)]
      (cond
        (= :deadline after)
        (assoc v :expires-on-deadline true)

        ;; ISO-shaped strings are dates (parse-iso-date also validates
        ;; the calendar); anything else is tried as a duration.
        (and (string? after) (re-matches #"\d{4}-\d{2}-\d{2}" after))
        (if-let [d (common/parse-iso-date after)]
          (assoc v :expires-on-date d)
          ;; Well-shaped but calendar-invalid ("2026-02-30"): a
          ;; silent nil would just never expire anything.
          (log/warn "Ignoring expiry rule with invalid calendar date:"
                    (pr-str after)))

        :else
        (when-let [d (parse-delay-safe after)]
          (assoc v :delay-days d))))))

(defn- report-activity-score
  "Activity score: acked=1, owned=2 (range 0-3, open-only)."
  [report]
  (+ (if (:report/owned report) 2 0)
     (if (:report/acked report) 1 0)))

(defn- rule-matches?
  "Check whether a report matches all expiry rule conditions.  A delay
  rule needs strictly more than `delay-days` whole days of inactivity
  (\"0d\" = after one full day; the Expiry: command path expires at
  the deadline itself)."
  [rule report-data now]
  (let [{:keys [delay-days expires-on-deadline expires-on-date max-status max-priority]} rule
        last-activity (:report/last-activity report-data)]
    (and
     ;; Age check: deadline-based, fixed date, or delay from last activity
     (cond
       expires-on-deadline
       (when-let [deadline (:report/deadline-value report-data)]
         (.before ^Date deadline now))
       expires-on-date
       (.before ^Date expires-on-date now)
       :else
       ;; .before guards against a forged future Date: header --
       ;; days-between is absolute, so "future" would look "old".
       (and delay-days last-activity
            (.before ^Date last-activity now)
            (> (common/days-between last-activity now) delay-days)))
     ;; Status ceiling (activity score: acked=1 + owned=2, range 0-3)
     (or (nil? max-status)
         (<= (report-activity-score report-data) max-status))
     ;; Priority ceiling
     (or (nil? max-priority)
         (<= (common/report-priority report-data) max-priority)))))

;; ---------------------------------------------------------------------------
;; Expiry engine
;; ---------------------------------------------------------------------------

(defn- find-or-create-expiry-email!
  "Get or create the synthetic email for an expiry event.  Uses a
  fresh (d/db conn) -- not a snapshot -- so re-runs within the same
  reduce see earlier inserts."
  [conn src report-mid now]
  (let [stripped  (str/replace (or report-mid "") #"^<|>$" "")
        synth-mid (str "<bone-expired-" stripped ">")]
    (or (lookup/email-eid (d/db conn) synth-mid)
        (let [tempid -1
              ;; Stamp "bone-system" on both :from-address and
              ;; :author-address so all consumers see the synthetic actor.
              tx (d/transact!
                  conn [{:db/id                  tempid
                         :email/message-id       synth-mid
                         :email/message-id-hash  (common/mid-hash synth-mid)
                         :email/from-address     "bone-system"
                         :email/author-address   "bone-system"
                         :email/source           src
                         :email/date-sent        now
                         :email/subject          (str "Auto-expired: " report-mid)}])]
          (get (:tempids tx) tempid)))))

(defn- should-expire?
  "True when a report matches its explicit :expiry-value or the
  source-level expiry rule for its type."
  [report-data source-map src rtype now]
  (let [explicit-expiry (:report/expiry-value report-data)]
    (if explicit-expiry
      (.before ^Date explicit-expiry now)
      (let [expiry-cfg (:expiry (get source-map src))
            rule-raw   (get expiry-cfg rtype)]
        (when-let [rule (parse-expiry-rule rule-raw)]
          (rule-matches? rule report-data now))))))

(defn- filter-expirable
  "Seq of {:rid :rtype :src :report-mid} for the open-report
  `candidates` matching their expiry rule."
  [candidates db-snap source-map now]
  (keep (fn [[rid rtype src]]
          (let [report-data (d/pull db-snap [:report/message-id
                                             :report/acked :report/owned
                                             :report/urgent :report/important
                                             :report/expiry-value :report/deadline-value
                                             :report/last-activity] rid)]
            (when (should-expire? report-data source-map src rtype now)
              {:rid rid :rtype rtype :src src
               :report-mid (:report/message-id report-data)})))
        candidates))

(defn expire-reports!
  "Close open reports matching the source's :expiry rule.  Sets
  :report/close-reason :expired."
  [conn source-map]
  (let [now (Date.)
        ;; One snapshot for query and pulls, so they can't disagree.
        db  (d/db conn)
        candidates (d/q '[:find ?r ?type ?src
                          :where
                          [?r :report/type ?type]
                          [?r :report/email ?e]
                          [?e :email/source ?src]
                          (not [?r :report/closed _])]
                        db)
        to-expire (filter-expirable candidates db source-map now)
        expired-rids (reduce
                      (fn [acc {:keys [rid rtype src report-mid]}]
                        (let [synth-eid (find-or-create-expiry-email! conn src report-mid now)]
                          (d/transact! conn [[:db/add rid :report/closed synth-eid]
                                             [:db/add rid :report/closed-address "bone-system"]
                                             [:db/add rid :report/close-reason :expired]])
                          (log/info "Expired" (name rtype) "report:" report-mid)
                          (conj acc rid)))
                      [] to-expire)]
    (when (seq expired-rids)
      ;; One bump for the whole batch.
      (tracking/bump-report-updated! conn expired-rids)
      (log/info "Expired" (count expired-rids) "report(s)."))))
