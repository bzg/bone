(ns bone.roles-test
  "Unit tests for the permission invariants of bone.roles/apply-role-controls!.
  These pin down the three rules that hold the new tenure model together:
    1. any active maintainer can Add maintainers
    2. only the lead maintainer can Remove maintainers
    3. the lead cannot remove themselves"
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [datalevin.core :as d]
            [bone.common :as common]
            [bone.roles :as roles]
            [bone.test-helpers :as th]))

(use-fixtures :once th/with-temp-failures-file)

;; fresh-conn registers each connection here so the :each fixture can
;; close it and delete its /tmp directory -- without it every run leaked
;; one directory and one open connection per call site.
(def ^:private live-conns (atom []))

(defn- fresh-conn []
  (let [path (str "/tmp/bone-roles-test-" (System/nanoTime))
        conn (d/get-conn path common/bone-schema)]
    (swap! live-conns conj {:conn conn :db-path path})
    conn))

(defn- with-conn-cleanup [f]
  (try (f)
       (finally
         (doseq [c @live-conns] (th/teardown! c))
         (reset! live-conns []))))

(use-fixtures :each with-conn-cleanup)

(defn- seed-two-maintainers!
  "Seed two maintainers on source `src`; the first is the lead."
  [conn src lead co-maint]
  (d/transact! conn [{:maint-tenure/source src
                      :maint-tenure/email  lead
                      :maint-tenure/order  0}
                     {:maint-tenure/source src
                      :maint-tenure/email  co-maint
                      :maint-tenure/order  1}]))

(def ^:private t1 #inst "2026-02-01T10:00:00.000-00:00")

(deftest add-maintainer-permissions
  (testing "a non-maintainer cannot Add"
    (let [conn (fresh-conn)
          _    (seed-two-maintainers! conn "s" "lead@x.org" "co@x.org")
          t0   (roles/get-tenures (d/db conn) "s")]
      (roles/apply-role-controls! conn t0 "s" "user@x.org"
                                  "Add maintainer: intruder@x.org" t1 false)
      (is (not (common/maintainer? (roles/get-tenures (d/db conn) "s")
                                   "intruder@x.org")))))

  (testing "a non-lead maintainer CAN Add"
    (let [conn (fresh-conn)
          _    (seed-two-maintainers! conn "s" "lead@x.org" "co@x.org")
          t0   (roles/get-tenures (d/db conn) "s")]
      (roles/apply-role-controls! conn t0 "s" "co@x.org"
                                  "Add maintainer: peer@x.org" t1 false)
      (is (common/maintainer? (roles/get-tenures (d/db conn) "s")
                              "peer@x.org")))))

(deftest remove-maintainer-permissions
  (testing "a non-lead maintainer CANNOT Remove"
    (let [conn (fresh-conn)
          _    (seed-two-maintainers! conn "s" "lead@x.org" "co@x.org")
          t0   (roles/get-tenures (d/db conn) "s")]
      (roles/apply-role-controls! conn t0 "s" "co@x.org"
                                  "Remove maintainer: lead@x.org" t1 false)
      ;; lead tenure is untouched
      (is (common/maintainer? (roles/get-tenures (d/db conn) "s")
                              "lead@x.org"))))

  (testing "the lead CAN remove a peer"
    (let [conn (fresh-conn)
          _    (seed-two-maintainers! conn "s" "lead@x.org" "co@x.org")
          t0   (roles/get-tenures (d/db conn) "s")]
      (roles/apply-role-controls! conn t0 "s" "lead@x.org"
                                  "Remove maintainer: co@x.org" t1 false)
      (is (not (common/maintainer? (roles/get-tenures (d/db conn) "s")
                                   "co@x.org"))))))

(deftest lead-is-irremovable
  (testing "the lead cannot remove themselves"
    (let [conn (fresh-conn)
          _    (seed-two-maintainers! conn "s" "lead@x.org" "co@x.org")
          t0   (roles/get-tenures (d/db conn) "s")]
      (roles/apply-role-controls! conn t0 "s" "lead@x.org"
                                  "Remove maintainer: lead@x.org" t1 false)
      (is (common/maintainer? (roles/get-tenures (d/db conn) "s")
                              "lead@x.org"))
      (is (= "lead@x.org"
             (common/lead-maintainer (roles/get-tenures (d/db conn) "s")))))))

(deftest rfc5322-address-forms
  (testing "plain bare address"
    (let [conn (fresh-conn)
          _    (seed-two-maintainers! conn "s" "lead@x.org" "co@x.org")
          t0   (roles/get-tenures (d/db conn) "s")]
      (roles/apply-role-controls! conn t0 "s" "co@x.org"
                                  "Add maintainer: peer@x.org" t1 false)
      (is (common/maintainer? (roles/get-tenures (d/db conn) "s")
                              "peer@x.org"))))

  (testing "Display Name <addr> form"
    (let [conn (fresh-conn)
          _    (seed-two-maintainers! conn "s" "lead@x.org" "co@x.org")
          t0   (roles/get-tenures (d/db conn) "s")]
      (roles/apply-role-controls! conn t0 "s" "co@x.org"
                                  "Add maintainer: Peer User <peer@x.org>" t1 false)
      (is (common/maintainer? (roles/get-tenures (d/db conn) "s")
                              "peer@x.org"))))

  (testing "mix of bracketed and bare addresses preserves order"
    (let [conn (fresh-conn)
          _    (seed-two-maintainers! conn "s" "lead@x.org" "co@x.org")
          t0   (roles/get-tenures (d/db conn) "s")]
      (roles/apply-role-controls!
       conn t0 "s" "co@x.org"
       "Add maintainer: alice@x.org Bob <bob@x.org> carol@x.org" t1 false)
      (let [ts (roles/get-tenures (d/db conn) "s")]
        (is (common/maintainer? ts "alice@x.org"))
        (is (common/maintainer? ts "bob@x.org"))
        (is (common/maintainer? ts "carol@x.org"))))))

;; ---------------------------------------------------------------------------
;; sync-source-tenures! -- per-period reconciliation
;; ---------------------------------------------------------------------------

(defn- emails [tenures]
  (set (map :email tenures)))

(defn- active [conn src]
  (emails (filter #(nil? (:to %)) (roles/get-tenures (d/db conn) src))))

(deftest sync-no-period-opens-with-nil-from
  (let [conn (fresh-conn)]
    (roles/sync-source-tenures! conn {:name "s" :maintainers ["a@x.org" "b@x.org"]})
    (is (= #{"a@x.org" "b@x.org"} (active conn "s")))
    (testing "all tenures open with :from = nil (unbounded past)"
      (is (every? #(nil? (:from %))
                  (roles/get-tenures (d/db conn) "s"))))))

(deftest sync-multi-period-opens-and-closes-at-boundaries
  (let [conn (fresh-conn)]
    (roles/sync-source-tenures!
     conn
     {:name    "s"
      :periods [{:end "2020-01-01" :maintainers ["a@x.org" "b@x.org"]}
                {:start "2020-01-01" :maintainers ["a@x.org" "c@x.org"]}]})
    (let [ts (roles/get-tenures (d/db conn) "s")]
      (is (= #{"a@x.org" "c@x.org"} (emails (filter #(nil? (:to %)) ts)))
          "active at end-of-run = declared by last period")
      (is (some #(and (= "b@x.org" (:email %))
                      (nil? (:from %))
                      (= (common/parse-iso-date "2020-01-01") (:to %))) ts)
          "b@x.org opened in era-1 then closed at era-2 start"))))

(deftest sync-is-idempotent
  (let [conn (fresh-conn)
        src  {:name "s"
              :periods [{:end "2020-01-01" :maintainers ["a@x.org"]}
                        {:start "2020-01-01" :maintainers ["b@x.org"]}]}]
    (roles/sync-source-tenures! conn src)
    (let [before (roles/get-tenures (d/db conn) "s")]
      (roles/sync-source-tenures! conn src)
      (let [after (roles/get-tenures (d/db conn) "s")]
        (is (= (count before) (count after))
            "re-running sync does not create duplicate tenures")
        (is (= (emails before) (emails after)))))))

(deftest sync-warm-restart-preserves-mail-removal
  ;; A mail directive closed bob's tenure. A warm re-sync with config
  ;; still declaring bob does NOT reinstate bob -- the mail action is
  ;; authoritative. To replay from scratch (ignoring historical mail
  ;; actions), the operator must use --fresh.
  (let [conn (fresh-conn)]
    (roles/sync-source-tenures!
     conn {:name "s" :maintainers ["lead@x.org" "bob@x.org"]})
    (let [t0 (roles/get-tenures (d/db conn) "s")]
      (roles/apply-role-controls!
       conn t0 "s" "lead@x.org" "Remove maintainer: bob@x.org" t1 false))
    (roles/sync-source-tenures!
     conn {:name "s" :maintainers ["lead@x.org" "bob@x.org"]})
    (is (not (contains? (active conn "s") "bob@x.org"))
        "bob stays removed -- warm re-sync is append-only here")))

(deftest sync-no-period-cannot-close-undeclared
  ;; Without :periods, sync is append-only (F=nil cannot carry a close
  ;; date). A pre-seeded maintainer absent from the declared list stays
  ;; active -- operator must use --fresh or add :periods.
  (let [conn (fresh-conn)]
    (seed-two-maintainers! conn "s" "lead@x.org" "bob@x.org")
    (roles/sync-source-tenures!
     conn {:name "s" :maintainers ["lead@x.org"]})
    (is (contains? (active conn "s") "bob@x.org")
        "bob stays active -- no :periods means no close date available")))

(deftest sync-future-period-does-not-preempt
  ;; A planned handover (:start in the future) must not close the
  ;; current maintainers' tenures before the boundary arrives: a
  ;; future :to would read as "no longer active" to every consumer
  ;; of active tenures, revoking the old lead's rights immediately.
  (let [conn   (fresh-conn)
        future (common/format-date-iso
                (java.util.Date. (+ (System/currentTimeMillis)
                                    (* 30 86400000))))]
    (roles/sync-source-tenures!
     conn
     {:name    "s"
      :periods [{:end future :maintainers ["old@x.org" "co@x.org"]}
                {:start future :maintainers ["new@x.org"]}]})
    (is (= #{"old@x.org" "co@x.org"} (active conn "s"))
        "the future boundary neither closes nor opens anything yet")
    (is (common/lead-maintainer? (roles/get-tenures (d/db conn) "s")
                                 "old@x.org")
        "the old lead keeps their rights until the handover date")))

(deftest sync-with-started-first-period-can-close
  ;; First period has :start, so a pre-existing mail-added maintainer
  ;; absent from the declared list IS closed.
  (let [conn (fresh-conn)]
    (seed-two-maintainers! conn "s" "lead@x.org" "bob@x.org")
    (roles/sync-source-tenures!
     conn {:name    "s"
           :periods [{:start "2020-01-01" :maintainers ["lead@x.org"]}]})
    (is (not (contains? (active conn "s") "bob@x.org"))
        "bob is closed at the first period's :start")))

;; ---------------------------------------------------------------------------
;; can-create-report? -- :restricted-types
;; ---------------------------------------------------------------------------

(def ^:private an-email {:email/date-sent t1})

(defn- can-create?
  "Helper: ask if `from-addr` (alone, no tenures) can create a report
   of `rtype` against `source-cfg`."
  [source-cfg from-addr rtype]
  (roles/can-create-report? [] from-addr {:type rtype} an-email source-cfg))

(deftest can-create-report-defaults
  (testing "Default restricted set covers announcement/release/change"
    (is (false? (can-create? {} "user@x.org" :announcement)))
    (is (false? (can-create? {} "user@x.org" :release)))
    (is (false? (can-create? {} "user@x.org" :change))))
  (testing "Other types pass by default"
    (is (true? (can-create? {} "user@x.org" :bug)))
    (is (true? (can-create? {} "user@x.org" :patch)))
    (is (true? (can-create? {} "user@x.org" :request)))))

(deftest can-create-report-empty-set-opens-everything
  (testing ":restricted-types #{} lets any sender create any type"
    (let [cfg {:restricted-types #{}}]
      (doseq [rtype [:bug :patch :request :announcement :release :change]]
        (is (true? (can-create? cfg "user@x.org" rtype))
            (str rtype " is open with empty :restricted-types"))))))

(deftest can-create-report-custom-set
  (testing "Custom restricted set restricts and opens types as declared"
    (let [cfg {:restricted-types #{:bug}}]
      ;; :bug now requires maintainer status -- non-maintainer denied.
      (is (false? (can-create? cfg "user@x.org" :bug))
          ":bug is now restricted")
      ;; :announcement is no longer in the set -- anyone can create it.
      (is (true? (can-create? cfg "user@x.org" :announcement))
          ":announcement is now open"))))

(deftest can-create-report-maintainer-always-passes
  (testing "An active maintainer can create any restricted type"
    (let [tenures [{:email "lead@x.org" :from nil :to nil}]]
      (is (true? (roles/can-create-report?
                  tenures "lead@x.org" {:type :announcement} an-email {})))
      (is (true? (roles/can-create-report?
                  tenures "lead@x.org" {:type :bug}
                  an-email {:restricted-types #{:bug}}))))))
