(ns bone.expire-test
  "Tests for bone.expire -- rule-based report expiry."
  (:require [clojure.test :refer [deftest is testing]]
            [datalevin.core :as d]
            [bone.common :as common]
            [bone.lookup :as lookup]
            [bone.expire :as expire]
            [bone.test-helpers :as th])
  (:import [java.util Date]))

;; ---------------------------------------------------------------------------
;; Helpers
;; ---------------------------------------------------------------------------

(defn- days-ago
  "Return a Date n days in the past."
  [n]
  (Date. (- (.getTime (Date.)) (* n 86400000))))

(defn- days-from-now
  "Return a Date n days in the future."
  [n]
  (Date. (+ (.getTime (Date.)) (* n 86400000))))

(defn- setup-db! []
  (let [db-path (str "/tmp/bone-expire-test-" (System/currentTimeMillis))
        conn    (d/get-conn db-path common/bone-schema)]
    {:conn conn :db-path db-path}))

(def ^:private teardown! th/teardown!)

(defn- insert-email!
  "Insert a test email and return its eid."
  [conn {:keys [mid from source date-sent subject]
         :or   {from "user@test.org" source "test-src"
                subject "Test" date-sent (days-ago 0)}}]
  (let [tempid -1
        tx (d/transact! conn [{:db/id                  tempid
                                :email/message-id       mid
                                :email/message-id-hash  (common/mid-hash mid)
                                :email/from-address     from
                                :email/author-address   from
                                :email/source           source
                                :email/date-sent        date-sent
                                :email/subject          subject}])]
    (get (:tempids tx) tempid)))

(defn- insert-report!
  "Insert a report linked to an email. Returns report eid."
  [conn {:keys [mid type email-eid date-sent]}]
  (let [email    (d/pull (d/db conn) [:email/date-sent :email/author-address] email-eid)
        activity (or date-sent (:email/date-sent email) (Date.))
        from     (:email/author-address email)]
    (d/transact! conn [(cond-> {:report/type            type
                                :report/email           email-eid
                                :report/message-id      mid
                                :report/message-id-hash (common/mid-hash mid)
                                :report/last-activity   activity}
                         from (assoc :report/last-activity-address from))])
    (lookup/report-eid (d/db conn) mid)))

(defn- set-report-state!
  "Set state attributes on a report (acked, owned, urgent, important)."
  [conn report-eid email-eid attrs]
  (let [tx (mapv (fn [attr] [:db/add report-eid attr email-eid]) attrs)]
    (when (seq tx) (d/transact! conn tx))))

(defn- add-descendant!
  "Add a descendant email to a report, updating :report/last-activity."
  [conn report-eid email-eid]
  (let [email      (d/pull (d/db conn) [:email/date-sent :email/author-address] email-eid)
        email-date (:email/date-sent email)
        from-addr  (:email/author-address email)
        current    (:report/last-activity (d/pull (d/db conn) [:report/last-activity] report-eid))
        tx         [[:db/add report-eid :report/descendants email-eid]]
        tx         (if (and email-date (or (nil? current) (.after ^Date email-date ^Date current)))
                     (cond-> (conj tx [:db/add report-eid :report/last-activity email-date])
                       from-addr (conj [:db/add report-eid :report/last-activity-address from-addr]))
                     tx)]
    (d/transact! conn tx)))

(defn- report-closed? [conn mid]
  (some? (:report/closed
           (d/pull (d/db conn)
                   [:report/closed]
                   (lookup/report-eid (d/db conn) mid)))))

;; ---------------------------------------------------------------------------
;; Tests
;; ---------------------------------------------------------------------------

(deftest delay-basic-test
  (testing "Report older than delay is expired"
    (let [{:keys [conn] :as ctx} (setup-db!)
          source-map {"test-src" {:expiry {:bug {:inactive-after"10d"}}}}
          eid (insert-email! conn {:mid "<old@test>" :date-sent (days-ago 15)})
          _   (insert-report! conn {:mid "<old@test>" :type :bug :email-eid eid})]
      (expire/expire-reports! conn source-map)
      (is (report-closed? conn "<old@test>"))
      (teardown! ctx)))

  (testing "Report younger than delay is NOT expired"
    (let [{:keys [conn] :as ctx} (setup-db!)
          source-map {"test-src" {:expiry {:bug {:inactive-after"10d"}}}}
          eid (insert-email! conn {:mid "<young@test>" :date-sent (days-ago 5)})
          _   (insert-report! conn {:mid "<young@test>" :type :bug :email-eid eid})]
      (expire/expire-reports! conn source-map)
      (is (not (report-closed? conn "<young@test>")))
      (teardown! ctx)))

  (testing "Delay computed from last descendant, not founding email"
    (let [{:keys [conn] :as ctx} (setup-db!)
          source-map {"test-src" {:expiry {:bug {:inactive-after"10d"}}}}
          eid (insert-email! conn {:mid "<old-thread@test>" :date-sent (days-ago 30)})
          rid (insert-report! conn {:mid "<old-thread@test>" :type :bug :email-eid eid})
          ;; Recent reply resets the clock
          reply-eid (insert-email! conn {:mid "<recent-reply@test>"
                                          :from "someone@test.org"
                                          :date-sent (days-ago 3)})]
      (add-descendant! conn rid reply-eid)
      (expire/expire-reports! conn source-map)
      (is (not (report-closed? conn "<old-thread@test>")))
      (teardown! ctx))))

(deftest delay-integer-in-map-test
  (testing "Integer :delay value inside a rule map"
    (let [{:keys [conn] :as ctx} (setup-db!)
          source-map {"test-src" {:expiry {:announcement {:inactive-after 10}}}}
          eid (insert-email! conn {:mid "<intdelay@test>" :date-sent (days-ago 15)})
          _   (insert-report! conn {:mid "<intdelay@test>" :type :announcement :email-eid eid})]
      (expire/expire-reports! conn source-map)
      (is (report-closed? conn "<intdelay@test>"))
      (teardown! ctx))))

(deftest delay-duration-formats-test
  (testing "Weeks duration"
    (let [{:keys [conn] :as ctx} (setup-db!)
          source-map {"test-src" {:expiry {:release {:inactive-after"2w"}}}}
          eid (insert-email! conn {:mid "<weeks@test>" :date-sent (days-ago 20)})
          _   (insert-report! conn {:mid "<weeks@test>" :type :release :email-eid eid})]
      (expire/expire-reports! conn source-map)
      (is (report-closed? conn "<weeks@test>"))
      (teardown! ctx)))

  (testing "Months duration"
    (let [{:keys [conn] :as ctx} (setup-db!)
          source-map {"test-src" {:expiry {:change {:inactive-after"1m"}}}}
          ;; 1m = 30 days, report is 35 days old
          eid (insert-email! conn {:mid "<months@test>" :date-sent (days-ago 35)})
          _   (insert-report! conn {:mid "<months@test>" :type :change :email-eid eid})]
      (expire/expire-reports! conn source-map)
      (is (report-closed? conn "<months@test>"))
      (teardown! ctx))))

(deftest max-status-test
  (testing "Unacked unowned report (score=0) expires when max-status=0"
    (let [{:keys [conn] :as ctx} (setup-db!)
          source-map {"test-src" {:expiry {:bug {:inactive-after"5d" :max-status 0}}}}
          eid (insert-email! conn {:mid "<s0-yes@test>" :date-sent (days-ago 10)})
          _   (insert-report! conn {:mid "<s0-yes@test>" :type :bug :email-eid eid})]
      (expire/expire-reports! conn source-map)
      (is (report-closed? conn "<s0-yes@test>"))
      (teardown! ctx)))

  (testing "Acked report (score=1) does NOT expire when max-status=0"
    (let [{:keys [conn] :as ctx} (setup-db!)
          source-map {"test-src" {:expiry {:bug {:inactive-after"5d" :max-status 0}}}}
          eid (insert-email! conn {:mid "<s1-no@test>" :date-sent (days-ago 10)})
          rid (insert-report! conn {:mid "<s1-no@test>" :type :bug :email-eid eid})
          ack-eid (insert-email! conn {:mid "<ack@test>" :from "maint@test.org"})
          _   (set-report-state! conn rid ack-eid [:report/acked])]
      (expire/expire-reports! conn source-map)
      (is (not (report-closed? conn "<s1-no@test>")))
      (teardown! ctx)))

  (testing "Owned report (score=2) does NOT expire when max-status=1"
    (let [{:keys [conn] :as ctx} (setup-db!)
          source-map {"test-src" {:expiry {:bug {:inactive-after"5d" :max-status 1}}}}
          eid (insert-email! conn {:mid "<s2-no@test>" :date-sent (days-ago 10)})
          rid (insert-report! conn {:mid "<s2-no@test>" :type :bug :email-eid eid})
          own-eid (insert-email! conn {:mid "<own@test>" :from "maint@test.org"})
          _   (set-report-state! conn rid own-eid [:report/owned])]
      (expire/expire-reports! conn source-map)
      (is (not (report-closed? conn "<s2-no@test>")))
      (teardown! ctx)))

  (testing "Acked report (score=1) expires when max-status=1"
    (let [{:keys [conn] :as ctx} (setup-db!)
          source-map {"test-src" {:expiry {:bug {:inactive-after"5d" :max-status 1}}}}
          eid (insert-email! conn {:mid "<s1-yes@test>" :date-sent (days-ago 10)})
          rid (insert-report! conn {:mid "<s1-yes@test>" :type :bug :email-eid eid})
          ack-eid (insert-email! conn {:mid "<ack2@test>" :from "maint@test.org"})
          _   (set-report-state! conn rid ack-eid [:report/acked])]
      (expire/expire-reports! conn source-map)
      (is (report-closed? conn "<s1-yes@test>"))
      (teardown! ctx))))

(deftest max-priority-test
  (testing "Non-urgent non-important report (priority=0) expires when max-priority >= 0"
    (let [{:keys [conn] :as ctx} (setup-db!)
          source-map {"test-src" {:expiry {:bug {:inactive-after"5d" :max-priority 0}}}}
          eid (insert-email! conn {:mid "<p0@test>" :date-sent (days-ago 10)})
          _   (insert-report! conn {:mid "<p0@test>" :type :bug :email-eid eid})]
      (expire/expire-reports! conn source-map)
      (is (report-closed? conn "<p0@test>"))
      (teardown! ctx)))

  (testing "Urgent report (priority=2) does NOT expire when max-priority=0"
    (let [{:keys [conn] :as ctx} (setup-db!)
          source-map {"test-src" {:expiry {:bug {:inactive-after"5d" :max-priority 0}}}}
          eid (insert-email! conn {:mid "<p2@test>" :date-sent (days-ago 10)})
          rid (insert-report! conn {:mid "<p2@test>" :type :bug :email-eid eid})
          urg-eid (insert-email! conn {:mid "<urg@test>" :from "maint@test.org"})
          _   (set-report-state! conn rid urg-eid [:report/urgent])]
      (expire/expire-reports! conn source-map)
      (is (not (report-closed? conn "<p2@test>")))
      (teardown! ctx))))

(deftest after-deadline-test
  (testing "Report with :inactive-after :deadline expires when deadline is past"
    (let [{:keys [conn] :as ctx} (setup-db!)
          source-map {"test-src" {:expiry {:bug {:inactive-after :deadline}}}}
          eid (insert-email! conn {:mid "<dl-past@test>" :date-sent (days-ago 10)})
          rid (insert-report! conn {:mid "<dl-past@test>" :type :bug :email-eid eid})
          _   (d/transact! conn [[:db/add rid :report/deadline-value (days-ago 2)]])]
      (expire/expire-reports! conn source-map)
      (is (report-closed? conn "<dl-past@test>"))
      (teardown! ctx)))

  (testing "Report with :inactive-after :deadline does NOT expire when deadline is future"
    (let [{:keys [conn] :as ctx} (setup-db!)
          source-map {"test-src" {:expiry {:bug {:inactive-after :deadline}}}}
          eid (insert-email! conn {:mid "<dl-future@test>" :date-sent (days-ago 10)})
          rid (insert-report! conn {:mid "<dl-future@test>" :type :bug :email-eid eid})
          _   (d/transact! conn [[:db/add rid :report/deadline-value (days-from-now 5)]])]
      (expire/expire-reports! conn source-map)
      (is (not (report-closed? conn "<dl-future@test>")))
      (teardown! ctx)))

  (testing "Report with :inactive-after :deadline but no deadline is NOT expired"
    (let [{:keys [conn] :as ctx} (setup-db!)
          source-map {"test-src" {:expiry {:bug {:inactive-after :deadline}}}}
          eid (insert-email! conn {:mid "<dl-none@test>" :date-sent (days-ago 100)})
          _   (insert-report! conn {:mid "<dl-none@test>" :type :bug :email-eid eid})]
      (expire/expire-reports! conn source-map)
      (is (not (report-closed? conn "<dl-none@test>")))
      (teardown! ctx))))

(deftest fixed-date-rule-test
  (testing "Rule with a past ISO :inactive-after date expires the report"
    (let [{:keys [conn] :as ctx} (setup-db!)
          source-map {"test-src" {:expiry {:bug {:inactive-after "2020-01-01"}}}}
          eid (insert-email! conn {:mid "<date-past@test>" :date-sent (days-ago 3)})
          _   (insert-report! conn {:mid "<date-past@test>" :type :bug :email-eid eid})]
      (expire/expire-reports! conn source-map)
      (is (report-closed? conn "<date-past@test>"))
      (teardown! ctx)))

  (testing "Rule with a future ISO :inactive-after date does NOT expire"
    (let [{:keys [conn] :as ctx} (setup-db!)
          source-map {"test-src" {:expiry {:bug {:inactive-after "2999-01-01"}}}}
          eid (insert-email! conn {:mid "<date-future@test>" :date-sent (days-ago 100)})
          _   (insert-report! conn {:mid "<date-future@test>" :type :bug :email-eid eid})]
      (expire/expire-reports! conn source-map)
      (is (not (report-closed? conn "<date-future@test>")))
      (teardown! ctx)))

  (testing "Calendar-invalid ISO date expires nothing (rule skipped)"
    (let [{:keys [conn] :as ctx} (setup-db!)
          source-map {"test-src" {:expiry {:bug {:inactive-after "2026-02-30"}}}}
          eid (insert-email! conn {:mid "<date-bad@test>" :date-sent (days-ago 100)})
          _   (insert-report! conn {:mid "<date-bad@test>" :type :bug :email-eid eid})]
      (expire/expire-reports! conn source-map)
      (is (not (report-closed? conn "<date-bad@test>")))
      (teardown! ctx))))

(deftest combined-rules-test
  (testing "All conditions met -- expires"
    (let [{:keys [conn] :as ctx} (setup-db!)
          source-map {"test-src" {:expiry {:bug {:inactive-after"5d"
                                                 :max-status 0
                                                 :max-priority 0}}}}
          eid (insert-email! conn {:mid "<combo-yes@test>" :date-sent (days-ago 10)
                                   :from "reporter@test.org"})
          rid (insert-report! conn {:mid "<combo-yes@test>" :type :bug :email-eid eid})
          reply (insert-email! conn {:mid "<combo-reply@test>"
                                     :from "other@test.org"
                                     :date-sent (days-ago 8)})]
      (add-descendant! conn rid reply)
      (expire/expire-reports! conn source-map)
      (is (report-closed? conn "<combo-yes@test>"))
      (teardown! ctx)))

  (testing "One condition fails (priority too high) -- does NOT expire"
    (let [{:keys [conn] :as ctx} (setup-db!)
          source-map {"test-src" {:expiry {:bug {:inactive-after"5d"
                                                 :max-status 0
                                                 :max-priority 0}}}}
          eid (insert-email! conn {:mid "<combo-no@test>" :date-sent (days-ago 10)
                                   :from "reporter@test.org"})
          rid (insert-report! conn {:mid "<combo-no@test>" :type :bug :email-eid eid})
          urg-eid (insert-email! conn {:mid "<combo-urg@test>" :from "maint@test.org"})
          reply (insert-email! conn {:mid "<combo-r@test>"
                                     :from "other@test.org"
                                     :date-sent (days-ago 8)})]
      (set-report-state! conn rid urg-eid [:report/urgent])
      (add-descendant! conn rid reply)
      (expire/expire-reports! conn source-map)
      (is (not (report-closed? conn "<combo-no@test>")))
      (teardown! ctx)))

  (testing "One condition fails (acked, score=1 > max-status=0) -- does NOT expire"
    (let [{:keys [conn] :as ctx} (setup-db!)
          source-map {"test-src" {:expiry {:bug {:inactive-after"5d"
                                                 :max-status 0
                                                 :max-priority 0}}}}
          eid (insert-email! conn {:mid "<combo-ack@test>" :date-sent (days-ago 10)
                                   :from "reporter@test.org"})
          rid (insert-report! conn {:mid "<combo-ack@test>" :type :bug :email-eid eid})
          ack-eid (insert-email! conn {:mid "<combo-a@test>" :from "maint@test.org"})
          reply (insert-email! conn {:mid "<combo-ar@test>"
                                     :from "other@test.org"
                                     :date-sent (days-ago 8)})]
      (set-report-state! conn rid ack-eid [:report/acked])
      (add-descendant! conn rid reply)
      (expire/expire-reports! conn source-map)
      (is (not (report-closed? conn "<combo-ack@test>")))
      (teardown! ctx))))

(deftest no-expiry-rule-test
  (testing "Report type with no expiry rule is never expired"
    (let [{:keys [conn] :as ctx} (setup-db!)
          source-map {"test-src" {:expiry {:announcement {:inactive-after"5d"}}}}
          eid (insert-email! conn {:mid "<norule@test>" :date-sent (days-ago 100)})
          _   (insert-report! conn {:mid "<norule@test>" :type :bug :email-eid eid})]
      (expire/expire-reports! conn source-map)
      (is (not (report-closed? conn "<norule@test>")))
      (teardown! ctx)))

  (testing "Source with no expiry config -- nothing expires"
    (let [{:keys [conn] :as ctx} (setup-db!)
          source-map {"test-src" {}}
          eid (insert-email! conn {:mid "<nocfg@test>" :date-sent (days-ago 100)})
          _   (insert-report! conn {:mid "<nocfg@test>" :type :bug :email-eid eid})]
      (expire/expire-reports! conn source-map)
      (is (not (report-closed? conn "<nocfg@test>")))
      (teardown! ctx))))

(deftest explicit-expiry-date-test
  (testing "Report with explicit :report/expiry in the past expires regardless of rules"
    (let [{:keys [conn] :as ctx} (setup-db!)
          ;; No expiry rules configured for this source
          source-map {"test-src" {}}
          eid (insert-email! conn {:mid "<explicit-yes@test>" :date-sent (days-ago 5)})
          rid (insert-report! conn {:mid "<explicit-yes@test>" :type :bug :email-eid eid})
          _   (d/transact! conn [[:db/add rid :report/expiry-value (days-ago 2)]])]
      (expire/expire-reports! conn source-map)
      (is (report-closed? conn "<explicit-yes@test>"))
      (teardown! ctx)))

  (testing "Report with explicit :report/expiry in the future does NOT expire"
    (let [{:keys [conn] :as ctx} (setup-db!)
          source-map {"test-src" {:expiry {:bug {:inactive-after"1d"}}}}
          eid (insert-email! conn {:mid "<explicit-no@test>" :date-sent (days-ago 10)})
          rid (insert-report! conn {:mid "<explicit-no@test>" :type :bug :email-eid eid})
          ;; Explicit expiry in the future overrides the global rule that would expire it
          _   (d/transact! conn [[:db/add rid :report/expiry-value (days-from-now 30)]])]
      (expire/expire-reports! conn source-map)
      (is (not (report-closed? conn "<explicit-no@test>")))
      (teardown! ctx))))

(deftest already-closed-not-expired-test
  (testing "Already closed report is not expired again"
    (let [{:keys [conn] :as ctx} (setup-db!)
          source-map {"test-src" {:expiry {:bug {:inactive-after"5d"}}}}
          eid (insert-email! conn {:mid "<closed@test>" :date-sent (days-ago 10)})
          rid (insert-report! conn {:mid "<closed@test>" :type :bug :email-eid eid})
          close-eid (insert-email! conn {:mid "<close-ev@test>" :from "maint@test.org"})
          _   (d/transact! conn [{:db/id rid
                                  :report/closed close-eid
                                  :report/close-reason :resolved}])]
      (expire/expire-reports! conn source-map)
      ;; Still closed with original reason, not overwritten to :expired
      (is (= :resolved (:report/close-reason
                          (d/pull (d/db conn) [:report/close-reason]
                                  (lookup/report-eid (d/db conn) "<closed@test>")))))
      (teardown! ctx))))

(defn- utc-midnight
  "UTC midnight, n days from today, as Deadline: and Expiry: are stored."
  [n]
  (Date/from (.toInstant (.atStartOfDay (.plusDays (java.time.LocalDate/now java.time.ZoneOffset/UTC) n)
                                        java.time.ZoneOffset/UTC))))

(deftest deadline-day-is-inclusive-test
  (testing "a deadline or expiry passes once its day is over"
    (doseq [[n closed?] [[0 false] [-1 true]]]
      (let [{:keys [conn] :as ctx} (setup-db!)
            source-map {"test-src" {:expiry {:bug {:inactive-after :deadline}}}}
            e1 (insert-email! conn {:mid "<dl@test>" :date-sent (days-ago 10)})
            r1 (insert-report! conn {:mid "<dl@test>" :type :bug :email-eid e1})
            e2 (insert-email! conn {:mid "<ex@test>" :date-sent (days-ago 10)})
            r2 (insert-report! conn {:mid "<ex@test>" :type :bug :email-eid e2})]
        (d/transact! conn [[:db/add r1 :report/deadline-value (utc-midnight n)]
                           [:db/add r2 :report/expiry-value (utc-midnight n)]])
        (expire/expire-reports! conn source-map)
        (is (= closed? (report-closed? conn "<dl@test>")) (str "Deadline: day " n))
        (is (= closed? (report-closed? conn "<ex@test>")) (str "Expiry: day " n))
        (teardown! ctx))))

  (testing "an ISO date in an :inactive-after rule is inclusive too"
    (let [today (java.time.LocalDate/now java.time.ZoneOffset/UTC)]
      (doseq [[date closed?] [[(str today) false]
                              [(str (.minusDays today 1)) true]]]
        (let [{:keys [conn] :as ctx} (setup-db!)
              mid (str "<rule-" date "@test>")
              source-map {"test-src" {:expiry {:bug {:inactive-after date}}}}
              eid (insert-email! conn {:mid mid :date-sent (days-ago 10)})
              _   (insert-report! conn {:mid mid :type :bug :email-eid eid})]
          (expire/expire-reports! conn source-map)
          (is (= closed? (report-closed? conn mid))
              (str ":inactive-after " date))
          (teardown! ctx))))))
