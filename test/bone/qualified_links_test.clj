;; Copyright (c) 2026 Bastien Guerry <bzg@gnu.org>
;; SPDX-License-Identifier: EPL-2.0
;; License-Filename: LICENSES/EPL-2.0.txt

(ns bone.qualified-links-test
  "Integration tests for the qualified-relations layer (:rel/*).
  Validates link-rel! behaviour: posts :related-to between threaded
  reports, and additionally :resolves when the new report is a patch
  in reply to a bug or request."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [datalevin.core :as d]
            [bone.commands :as commands]
            [bone.common :as common]
            [bone.lookup :as lookup]
            [bone.digest :as digest]
            [bone.relations :as rel]))

(defn- fresh-conn []
  (let [path (str "/tmp/bone-rel-test-" (System/currentTimeMillis) "-" (rand-int 1e6))
        conn (d/get-conn path common/bone-schema)]
    {:conn conn :path path}))

(defn- close-and-cleanup! [{:keys [conn path]}]
  (d/close conn)
  (let [dir (io/file path)]
    (when (.exists dir)
      (doseq [f (reverse (file-seq dir))]
        (.delete f)))))

(defn- mk-email!
  "Insert an email and return its eid."
  [conn mid author date]
  (d/transact! conn [{:email/id              mid
                      :email/source          "test"
                      :email/message-id      mid
                      :email/message-id-hash (common/mid-hash mid)
                      :email/subject         (str "Subject of " mid)
                      :email/author-address  author
                      :email/from-address    author
                      :email/date-sent       date
                      :email/ingested-at     date}])
  (lookup/email-eid (d/db conn) mid))

(defn- mk-report!
  "Insert a report tied to an email and return its eid."
  [conn mid email-eid type]
  (d/transact! conn [{:report/message-id      mid
                      :report/message-id-hash (common/mid-hash mid)
                      :report/type            type
                      :report/email           email-eid}])
  (lookup/report-eid (d/db conn) mid))

(defn- get-relations
  "Return all relations targeting OR sourced from the given report eid,
  as maps {:from :to :kind :active? :setter}."
  [db report-eid]
  (->> (d/q '[:find ?from ?to ?kind ?active ?setter
              :in $ ?r
              :where
              (or [?rel :rel/from ?r]
                  [?rel :rel/to ?r])
              [?rel :rel/from ?from]
              [?rel :rel/to ?to]
              [?rel :rel/kind ?kind]
              [?rel :rel/active? ?active]
              [?rel :rel/setter ?setter]]
            db report-eid)
       (mapv (fn [[f t k a s]]
               {:from f :to t :kind k :active? a :setter s}))))

;; ---------------------------------------------------------------------------
;; Tests
;; ---------------------------------------------------------------------------

(deftest link-rel!-patch-replies-to-bug
  (testing "patch posted in reply to bug => :related-to AND :resolves posted"
    (let [{:keys [conn] :as setup} (fresh-conn)]
      (try
        (let [bug-email   (mk-email! conn "<bug-1@x>" "alice@x" #inst "2026-01-01")
              bug-eid     (mk-report! conn "<bug-1@x>" bug-email :bug)
              patch-email-map {:db/id (mk-email! conn "<patch-1@x>" "bob@x" #inst "2026-01-02")
                               :email/author-address "bob@x"
                               :email/date-sent #inst "2026-01-02"}
              patch-eid   (mk-report! conn "<patch-1@x>" (:db/id patch-email-map) :patch)]
          (#'digest/link-rel! conn patch-eid :patch patch-email-map [bug-eid])
          (let [rels (get-relations (d/db conn) patch-eid)
                kinds (set (map :kind rels))]
            (is (contains? kinds :related-to)  ":related-to posted")
            (is (contains? kinds :resolves)    ":resolves posted (patch -> bug)")
            (is (contains? kinds :resolved-by) "inverse :resolved-by posted")
            (is (every? :active? rels)         "all posed relations are active")
            (is (every? #(= "bob@x" (:setter %)) rels)
                "setter is the patch author")))
        (finally (close-and-cleanup! setup))))))

(deftest link-rel!-bug-replies-to-bug
  (testing "bug posted in reply to another bug => :related-to only, no :resolves"
    (let [{:keys [conn] :as setup} (fresh-conn)]
      (try
        (let [parent-email (mk-email! conn "<bug-A@x>" "alice@x" #inst "2026-01-01")
              parent-eid   (mk-report! conn "<bug-A@x>" parent-email :bug)
              child-email-map {:db/id (mk-email! conn "<bug-B@x>" "alice@x" #inst "2026-01-02")
                               :email/author-address "alice@x"
                               :email/date-sent #inst "2026-01-02"}
              child-eid   (mk-report! conn "<bug-B@x>" (:db/id child-email-map) :bug)]
          (#'digest/link-rel! conn child-eid :bug child-email-map [parent-eid])
          (let [rels (get-relations (d/db conn) child-eid)
                kinds (set (map :kind rels))]
            (is (contains? kinds :related-to)
                ":related-to posted between two bugs")
            (is (not (contains? kinds :resolves))
                "no :resolves (bug doesn't resolve bug)")
            (is (not (contains? kinds :resolved-by))
                "no :resolved-by either")))
        (finally (close-and-cleanup! setup))))))

(deftest link-rel!-idempotent
  (testing "calling link-rel! twice with the same args does not duplicate"
    (let [{:keys [conn] :as setup} (fresh-conn)]
      (try
        (let [bug-email   (mk-email! conn "<bug-1@x>" "alice@x" #inst "2026-01-01")
              bug-eid     (mk-report! conn "<bug-1@x>" bug-email :bug)
              patch-email-map {:db/id (mk-email! conn "<patch-1@x>" "bob@x" #inst "2026-01-02")
                               :email/author-address "bob@x"
                               :email/date-sent #inst "2026-01-02"}
              patch-eid   (mk-report! conn "<patch-1@x>" (:db/id patch-email-map) :patch)]
          (#'digest/link-rel! conn patch-eid :patch patch-email-map [bug-eid])
          (#'digest/link-rel! conn patch-eid :patch patch-email-map [bug-eid])
          (let [rels (get-relations (d/db conn) patch-eid)
                kinds (frequencies (map :kind rels))]
            ;; expected: 1 :related-to (symmetric) + 1 :resolves + 1 :resolved-by = 3 datoms total
            (is (= 1 (kinds :related-to))  "exactly one :related-to datom")
            (is (= 1 (kinds :resolves))    "exactly one :resolves datom")
            (is (= 1 (kinds :resolved-by)) "exactly one :resolved-by datom")
            (is (= 3 (count rels))         "no duplicates after replay")))
        (finally (close-and-cleanup! setup))))))

;; ---------------------------------------------------------------------------
;; Phase 3: Superseded-by directive -- relation pose / retract
;; ---------------------------------------------------------------------------

(deftest superseded-by-posts-supersedes-relation
  (testing "Superseded-by: <mid> posts a :supersedes relation, closes source"
    (let [{:keys [conn] :as setup} (fresh-conn)]
      (try
        (let [tgt-mid    "<patch-v2@x>"
              src-mid    "<patch-v1@x>"
              tgt-email  (mk-email! conn tgt-mid "carol@x" #inst "2026-02-02")
              _tgt-eid   (mk-report! conn tgt-mid tgt-email :patch)
              src-email  (mk-email! conn src-mid "alice@x" #inst "2026-02-01")
              src-eid    (mk-report! conn src-mid src-email :patch)
              ;; "Reply" email carrying the directive
              cmd-email-eid (mk-email! conn "<cmd-1@x>" "bob@x" #inst "2026-02-03")
              cmd-email     {:db/id cmd-email-eid
                             :email/author-address "bob@x"
                             :email/date-sent #inst "2026-02-03"
                             :email/body-text (str "Hi.\n\nSuperseded-by: " tgt-mid "\n")}]
          ;; Apply directive on src-eid
          (commands/apply-commands! conn src-eid :patch cmd-email
                                    {} {} :direct nil)
          (let [src-after (d/pull (d/db conn)
                                  [:report/closed :report/close-reason] src-eid)
                rels (get-relations (d/db conn) src-eid)
                kinds (set (map :kind rels))]
            (is (some? (:report/closed src-after))
                "source report is closed")
            (is (= :superseded (:report/close-reason src-after))
                "close-reason is :superseded")
            (is (contains? kinds :supersedes)    ":supersedes posted")
            (is (contains? kinds :superseded-by) "inverse :superseded-by posted")
            (is (contains? kinds :related-to)
                ":related-to also posted (cohesion with link-rel!)")))
        (finally (close-and-cleanup! setup))))))

(deftest superseded-by-rejects-cross-type
  (testing "Superseded-by: ignored when source/target types differ"
    (let [{:keys [conn] :as setup} (fresh-conn)]
      (try
        (let [bug-mid   "<bug-1@x>"
              bug-email (mk-email! conn bug-mid "alice@x" #inst "2026-02-01")
              bug-eid   (mk-report! conn bug-mid bug-email :bug)
              patch-mid "<patch-1@x>"
              patch-email (mk-email! conn patch-mid "alice@x" #inst "2026-02-02")
              _patch-eid (mk-report! conn patch-mid patch-email :patch)
              cmd-email-eid (mk-email! conn "<cmd@x>" "bob@x" #inst "2026-02-03")
              cmd-email     {:db/id cmd-email-eid
                             :email/author-address "bob@x"
                             :email/date-sent #inst "2026-02-03"
                             :email/body-text (str "Superseded-by: " patch-mid "\n")}]
          (commands/apply-commands! conn bug-eid :bug cmd-email
                                    {} {} :direct nil)
          (let [src-after (d/pull (d/db conn)
                                  [:report/closed :report/close-reason] bug-eid)
                rels (get-relations (d/db conn) bug-eid)
                kinds (set (map :kind rels))]
            (is (nil? (:report/closed src-after))
                "bug not closed when target is a patch (different type)")
            (is (not (contains? kinds :supersedes))
                ":supersedes not posted")))
        (finally (close-and-cleanup! setup))))))

(deftest not-superseded-retracts-relation
  (testing "Not superseded-by: retracts the :supersedes relation and reopens"
    (let [{:keys [conn] :as setup} (fresh-conn)]
      (try
        (let [tgt-mid   "<patch-v2@x>"
              src-mid   "<patch-v1@x>"
              _tgt-email (mk-email! conn tgt-mid "carol@x" #inst "2026-02-02")
              _tgt-eid  (mk-report! conn tgt-mid
                                    (lookup/email-eid (d/db conn) tgt-mid)
                                    :patch)
              src-email (mk-email! conn src-mid "alice@x" #inst "2026-02-01")
              src-eid   (mk-report! conn src-mid src-email :patch)
              ;; Apply Superseded-by first (alice as setter)
              set-email-eid (mk-email! conn "<set@x>" "alice@x" #inst "2026-02-03")
              set-email {:db/id set-email-eid
                         :email/author-address "alice@x"
                         :email/date-sent #inst "2026-02-03"
                         :email/body-text (str "Superseded-by: " tgt-mid "\n")}
              _ (commands/apply-commands! conn src-eid :patch set-email
                                          {} {} :direct nil)
              ;; Now retract -- same setter (alice)
              unset-email-eid (mk-email! conn "<unset@x>" "alice@x" #inst "2026-02-04")
              unset-email {:db/id unset-email-eid
                           :email/author-address "alice@x"
                           :email/date-sent #inst "2026-02-04"
                           :email/body-text (str "Not superseded-by: " tgt-mid "\n")}]
          (commands/apply-commands! conn src-eid :patch unset-email
                                    {} {} :direct nil)
          (let [src-after (d/pull (d/db conn)
                                  [:report/closed :report/close-reason] src-eid)
                ;; All relations of kind :supersedes from src-eid should be inactive
                active-supersedes (d/q '[:find [?e ...] :in $ ?from
                                         :where
                                         [?e :rel/from ?from]
                                         [?e :rel/kind :supersedes]
                                         [?e :rel/active? true]]
                                       (d/db conn) src-eid)]
            (is (nil? (:report/closed src-after))
                "source report reopened")
            (is (nil? (:report/close-reason src-after))
                "close-reason cleared")
            (is (empty? active-supersedes)
                ":supersedes relation no longer active (retracted)")))
        (finally (close-and-cleanup! setup))))))

(deftest not-superseded-with-wrong-mid-is-a-no-op
  (testing "Not superseded-by: <wrong-mid> on a closed (superseded) report
            leaves the relation untouched -- only the explicit mid match
            retracts."
    (let [{:keys [conn] :as setup} (fresh-conn)]
      (try
        (let [tgt-mid    "<patch-v2@x>"
              other-mid  "<other-patch@x>"
              src-mid    "<patch-v1@x>"
              _tgt-eid   (mk-report! conn tgt-mid
                                     (mk-email! conn tgt-mid "carol@x" #inst "2026-02-02")
                                     :patch)
              _other-eid (mk-report! conn other-mid
                                     (mk-email! conn other-mid "dave@x" #inst "2026-02-02")
                                     :patch)
              src-eid    (mk-report! conn src-mid
                                     (mk-email! conn src-mid "alice@x" #inst "2026-02-01")
                                     :patch)
              ;; Alice supersedes src by tgt
              set-email-eid (mk-email! conn "<set@x>" "alice@x" #inst "2026-02-03")
              set-email {:db/id set-email-eid
                         :email/author-address "alice@x"
                         :email/date-sent #inst "2026-02-03"
                         :email/body-text (str "Superseded-by: " tgt-mid "\n")}
              _ (commands/apply-commands! conn src-eid :patch set-email
                                          {} {} :direct nil)
              ;; Alice tries to retract with the WRONG mid
              unset-email-eid (mk-email! conn "<unset@x>" "alice@x" #inst "2026-02-04")
              unset-email {:db/id unset-email-eid
                           :email/author-address "alice@x"
                           :email/date-sent #inst "2026-02-04"
                           :email/body-text (str "Not superseded-by: " other-mid "\n")}]
          (commands/apply-commands! conn src-eid :patch unset-email
                                    {} {} :direct nil)
          (let [src-after (d/pull (d/db conn)
                                  [:report/closed :report/close-reason] src-eid)
                active-supersedes (d/q '[:find [?e ...] :in $ ?from
                                         :where
                                         [?e :rel/from ?from]
                                         [?e :rel/kind :supersedes]
                                         [?e :rel/active? true]]
                                       (d/db conn) src-eid)]
            (is (some? (:report/closed src-after))
                "report stays closed -- wrong mid is a no-op")
            (is (= :superseded (:report/close-reason src-after))
                "close-reason untouched")
            (is (= 1 (count active-supersedes))
                ":supersedes relation still active (not retracted)")))
        (finally (close-and-cleanup! setup))))))

(deftest not-supersedes-clears-only-the-named-target
  (testing "Not supersedes: <mid> on the superseder retracts only the
            relation with the named report: another report superseded
            by the same superseder keeps its closure and its relation."
    (let [{:keys [conn] :as setup} (fresh-conn)]
      (try
        (let [mk-patch (fn [mid author date]
                         (mk-report! conn mid (mk-email! conn mid author date)
                                     :patch))
              a-eid    (mk-patch "<patch-a@x>" "alice@x" #inst "2026-05-01")
              b-eid    (mk-patch "<patch-b@x>" "bob@x"   #inst "2026-05-02")
              c-eid    (mk-patch "<patch-c@x>" "carol@x" #inst "2026-05-03")
              send!    (fn [rid mid date body]
                         (commands/apply-commands!
                          conn rid :patch
                          {:db/id (mk-email! conn mid "alice@x" date)
                           :email/author-address "alice@x"
                           :email/date-sent date
                           :email/body-text body}
                          {} {} :direct nil))
              active-supersedes
              (fn [from-eid]
                (d/q '[:find [?e ...] :in $ ?from
                       :where
                       [?e :rel/from ?from]
                       [?e :rel/kind :supersedes]
                       [?e :rel/active? true]]
                     (d/db conn) from-eid))]
          ;; B and C are both superseded by A.
          (send! b-eid "<sup-b@x>" #inst "2026-05-04"
                 "Superseded-by: <patch-a@x>\n")
          (send! c-eid "<sup-c@x>" #inst "2026-05-05"
                 "Superseded-by: <patch-a@x>\n")
          ;; On A, clear only the relation with B.
          (send! a-eid "<not-sup@x>" #inst "2026-05-06"
                 "Not supersedes: <patch-b@x>\n")
          (let [b-after (d/pull (d/db conn)
                                [:report/closed :report/close-reason] b-eid)
                c-after (d/pull (d/db conn)
                                [:report/closed :report/close-reason] c-eid)]
            (is (nil? (:report/closed b-after))       "B reopened")
            (is (nil? (:report/close-reason b-after)) "B close-reason cleared")
            (is (empty? (active-supersedes b-eid))
                "B's :supersedes relation retracted")
            (is (some? (:report/closed c-after))
                "C stays closed -- not collateral damage of B's clear")
            (is (= :superseded (:report/close-reason c-after))
                "C's close-reason untouched")
            (is (= 1 (count (active-supersedes c-eid)))
                "C's :supersedes relation to A stays active")))
        (finally (close-and-cleanup! setup))))))

(deftest superseded-by-self-loop-is-recorded
  (testing "Superseded-by: <own-mid> records a :self-loop failure and
            leaves the report open.  This catches the common pitfall
            where a new [BUG] reply names its own thread root as the
            superseder, which would otherwise no-op silently."
    (let [{:keys [conn] :as setup} (fresh-conn)]
      (try
        (let [bug-mid     "<bug-self@x>"
              bug-email   (mk-email! conn bug-mid "alice@x" #inst "2026-04-01")
              bug-eid     (mk-report! conn bug-mid bug-email :bug)
              cmd-eid     (mk-email! conn "<cmd-self@x>" "alice@x" #inst "2026-04-02")
              cmd-email   {:db/id cmd-eid
                           :email/author-address "alice@x"
                           :email/date-sent #inst "2026-04-02"
                           :email/body-text (str "Superseded-by: " bug-mid "\n")}
              recorded    (atom [])]
          (with-redefs [commands/record-failure! (fn [entry] (swap! recorded conj entry))]
            (commands/apply-commands! conn bug-eid :bug cmd-email
                                      {} {} :direct nil))
          (let [after (d/pull (d/db conn)
                              [:report/closed :report/close-reason] bug-eid)]
            (is (nil? (:report/closed after))
                "report stays open -- self-loop is rejected")
            (is (nil? (:report/close-reason after))
                "no close-reason")
            (is (some #(= :self-loop (:reason %)) @recorded)
                "a :self-loop failure was recorded")
            (let [entry (some #(when (= :self-loop (:reason %)) %) @recorded)]
              (is (= :author (:audience entry))
                  "self-loop is routed to the author (the typo culprit)")
              (is (= (str "Superseded-by: " bug-mid) (:command entry))))))
        (finally (close-and-cleanup! setup))))))

(deftest conflicting-closure-commands-last-wins
  (testing "Superseded-by: and Duplicate-of: in one email both close the
            report, so they are mutually exclusive: the last line wins
            (same arbitration as repeated lines of one command)."
    (doseq [[body expected-reason expected-kind]
            [[(fn [sup dup] (str "Superseded-by: " sup "\n"
                                 "Duplicate-of: "  dup "\n"))
              :canceled   :duplicates]
             [(fn [sup dup] (str "Duplicate-of: "  dup "\n"
                                 "Superseded-by: " sup "\n"))
              :superseded :supersedes]]]
      (let [{:keys [conn] :as setup} (fresh-conn)]
        (try
          (let [old-mid   "<patch-old-c@x>"
                sup-mid   "<patch-sup@x>"
                dup-mid   "<patch-dup-c@x>"
                old-email (mk-email! conn old-mid "alice@x" #inst "2026-04-01")
                old-eid   (mk-report! conn old-mid old-email :patch)
                sup-email (mk-email! conn sup-mid "alice@x" #inst "2026-04-02")
                _sup-eid  (mk-report! conn sup-mid sup-email :patch)
                dup-email (mk-email! conn dup-mid "alice@x" #inst "2026-04-03")
                _dup-eid  (mk-report! conn dup-mid dup-email :patch)
                cmd-eid   (mk-email! conn "<cmd-conflict@x>" "alice@x" #inst "2026-04-04")
                cmd-email {:db/id cmd-eid
                           :email/author-address "alice@x"
                           :email/date-sent #inst "2026-04-04"
                           :email/body-text (body sup-mid dup-mid)}]
            (commands/apply-commands! conn old-eid :patch cmd-email
                                      {} {} :direct nil)
            (let [after (d/pull (d/db conn)
                                [:report/closed :report/close-reason] old-eid)
                  kinds (set (map :kind (get-relations (d/db conn) old-eid)))]
              (is (some? (:report/closed after)) "report is closed")
              (is (= expected-reason (:report/close-reason after))
                  "close-reason follows the last line")
              (is (contains? kinds expected-kind)
                  "only the last line's relation is posed")
              (is (not (contains? kinds (case expected-kind
                                          :duplicates :supersedes
                                          :supersedes :duplicates)))
                  "the overridden line posed nothing")))
          (finally (close-and-cleanup! setup)))))))

(deftest closure-commands-on-closed-report-replace-closure
  (testing "Posing a closure relation on an already-closed report
            replaces the previous closure: reason, credit and relation
            follow the last command received, and the report keeps at
            most one active closure relation."
    (let [{:keys [conn] :as setup} (fresh-conn)]
      (try
        (let [mk-bug  (fn [mid author date]
                        (mk-report! conn mid (mk-email! conn mid author date) :bug))
              r-eid   (mk-bug "<bug-r@x>" "alice@x" #inst "2026-05-01")
              x-eid   (mk-bug "<bug-x@x>" "bob@x"   #inst "2026-05-02")
              y-eid   (mk-bug "<bug-y@x>" "carol@x" #inst "2026-05-03")
              d-eid   (mk-bug "<bug-d@x>" "dave@x"  #inst "2026-05-04")
              send!   (fn [mid date body]
                        (commands/apply-commands!
                         conn r-eid :bug
                         {:db/id (mk-email! conn mid "alice@x" date)
                          :email/author-address "alice@x"
                          :email/date-sent date
                          :email/body-text body}
                         {} {} :direct nil))
              active  (fn [kind]
                        (into #{} (comp (filter #(and (:active? %)
                                                      (= kind (:kind %))
                                                      (= r-eid (:from %))))
                                        (map :to))
                              (get-relations (d/db conn) r-eid)))
              state   (fn [] (d/pull (d/db conn)
                                     [:report/closed :report/close-reason] r-eid))]
          (send! "<c1@x>" #inst "2026-05-05" "Superseded-by: <bug-x@x>\n")
          (is (= :superseded (:report/close-reason (state))))
          (is (= #{x-eid} (active :supersedes)))

          (send! "<c2@x>" #inst "2026-05-06" "Superseded-by: <bug-y@x>\n")
          (is (some? (:report/closed (state))) "report stays closed")
          (is (= :superseded (:report/close-reason (state))))
          (is (= #{y-eid} (active :supersedes))
              "the new superseder replaces the old one")
          (is (= #{x-eid y-eid} (active :related-to))
              "the old :related-to companion stays")

          (send! "<c3@x>" #inst "2026-05-07" "Duplicate-of: <bug-d@x>\n")
          (is (= :canceled (:report/close-reason (state)))
              "close-reason follows the new relation kind")
          (is (= #{d-eid} (active :duplicates)))
          (is (empty? (active :supersedes))
              "the previous closure relation is retracted"))
        (finally (close-and-cleanup! setup))))))

(deftest not-closed-and-closure-pose-in-one-mail
  (testing "Not closed. combined with a closure pose in the same mail:
            the closure wins, whatever the line order."
    (doseq [body ["Not closed.\nSuperseded-by: <bug-yy@x>\n"
                  "Superseded-by: <bug-yy@x>\nNot closed.\n"]]
      (let [{:keys [conn] :as setup} (fresh-conn)]
        (try
          (let [mk-bug (fn [mid author date]
                         (mk-report! conn mid (mk-email! conn mid author date) :bug))
                r-eid  (mk-bug "<bug-rr@x>" "alice@x" #inst "2026-05-01")
                _x-eid (mk-bug "<bug-xx@x>" "bob@x"   #inst "2026-05-02")
                y-eid  (mk-bug "<bug-yy@x>" "carol@x" #inst "2026-05-03")
                send!  (fn [mid date text]
                         (commands/apply-commands!
                          conn r-eid :bug
                          {:db/id (mk-email! conn mid "alice@x" date)
                           :email/author-address "alice@x"
                           :email/date-sent date
                           :email/body-text text}
                          {} {} :direct nil))]
            (send! "<cc1@x>" #inst "2026-05-04" "Superseded-by: <bug-xx@x>\n")
            (send! "<cc2@x>" #inst "2026-05-05" body)
            (let [after  (d/pull (d/db conn)
                                 [:report/closed :report/close-reason] r-eid)
                  active (into #{} (comp (filter #(and (:active? %)
                                                       (= :supersedes (:kind %))
                                                       (= r-eid (:from %))))
                                         (map :to))
                               (get-relations (d/db conn) r-eid))]
              (is (some? (:report/closed after)) "report ends up closed")
              (is (= :superseded (:report/close-reason after)))
              (is (= #{y-eid} active)
                  "the closure relation follows the pose, not the reopen")))
          (finally (close-and-cleanup! setup)))))))

;; ---------------------------------------------------------------------------
;; Votes: one entity per voter per report, last vote wins
;; ---------------------------------------------------------------------------

(deftest revote-replaces-previous-vote
  (testing "A voter's new vote replaces the previous one; an identical
            re-vote is a no-op; voters stay independent."
    (let [{:keys [conn] :as setup} (fresh-conn)]
      (try
        (let [req-mid "<req-vote@x>"
              r-eid   (mk-report! conn req-mid
                                  (mk-email! conn req-mid "alice@x"
                                             #inst "2026-06-01")
                                  :request)
              vote!   (fn [mid author date text]
                        (let [eid (mk-email! conn mid author date)]
                          (commands/apply-commands!
                           conn r-eid :request
                           {:db/id eid
                            :email/author-address author
                            :email/date-sent date
                            :email/body-text text}
                           {"test" {:source-type :mailbox}} {} :direct nil)
                          eid))
              votes   (fn []
                        (d/q '[:find ?voter ?val ?e
                               :in $ ?r
                               :where
                               [?v :vote/report ?r]
                               [?v :vote/voter ?voter]
                               [?v :vote/value ?val]
                               [?v :vote/email ?e]]
                             (d/db conn) r-eid))]
          (vote! "<v1@x>" "bob@x" #inst "2026-06-02" "+1\n")
          (is (= #{["bob@x" :up]} (into #{} (map (juxt first second)) (votes))))

          (let [m2 (vote! "<v2@x>" "bob@x" #inst "2026-06-03" "-1\n")]
            (is (= #{["bob@x" :down m2]} (votes))
                "one entity per voter, value and email follow the last vote")

            (vote! "<v3@x>" "bob@x" #inst "2026-06-04" "-1\n")
            (is (= #{["bob@x" :down m2]} (votes))
                "an identical re-vote changes nothing"))

          (vote! "<v4@x>" "carol@x" #inst "2026-06-05" "+1\n")
          (is (= #{["bob@x" :down] ["carol@x" :up]}
                 (into #{} (map (juxt first second)) (votes)))
              "each voter keeps their own vote"))
        (finally (close-and-cleanup! setup))))))

;; ---------------------------------------------------------------------------
;; Phase 4 (partial): Duplicate-of directive
;; ---------------------------------------------------------------------------

(deftest duplicate-of-posts-duplicates-relation
  (testing "Duplicate-of: <mid> posts a :duplicates relation, closes with :canceled"
    (let [{:keys [conn] :as setup} (fresh-conn)]
      (try
        (let [orig-mid  "<bug-orig@x>"
              dup-mid   "<bug-dup@x>"
              orig-email (mk-email! conn orig-mid "alice@x" #inst "2026-03-01")
              _orig-eid  (mk-report! conn orig-mid orig-email :bug)
              dup-email  (mk-email! conn dup-mid "bob@x" #inst "2026-03-02")
              dup-eid    (mk-report! conn dup-mid dup-email :bug)
              cmd-email-eid (mk-email! conn "<cmd@x>" "carol@x" #inst "2026-03-03")
              cmd-email     {:db/id cmd-email-eid
                             :email/author-address "carol@x"
                             :email/date-sent #inst "2026-03-03"
                             :email/body-text (str "Duplicate-of: " orig-mid "\n")}]
          (commands/apply-commands! conn dup-eid :bug cmd-email
                                    {} {} :direct nil)
          (let [src-after (d/pull (d/db conn)
                                  [:report/closed :report/close-reason] dup-eid)
                rels (get-relations (d/db conn) dup-eid)
                kinds (set (map :kind rels))]
            (is (some? (:report/closed src-after)) "duplicate report is closed")
            (is (= :canceled (:report/close-reason src-after))
                "close-reason is :canceled (not :resolved)")
            (is (contains? kinds :duplicates)    ":duplicates posted")
            (is (contains? kinds :duplicated-by) "inverse :duplicated-by posted")
            (is (contains? kinds :related-to)
                ":related-to also posted (cohesion)")))
        (finally (close-and-cleanup! setup))))))

(deftest duplicate-of-rejects-cross-type
  (testing "Duplicate-of: ignored when source/target types differ"
    (let [{:keys [conn] :as setup} (fresh-conn)]
      (try
        (let [bug-mid    "<bug-1@x>"
              bug-email  (mk-email! conn bug-mid "alice@x" #inst "2026-03-01")
              _bug-eid   (mk-report! conn bug-mid bug-email :bug)
              req-mid    "<req-1@x>"
              req-email  (mk-email! conn req-mid "bob@x" #inst "2026-03-02")
              req-eid    (mk-report! conn req-mid req-email :request)
              cmd-email-eid (mk-email! conn "<cmd@x>" "carol@x" #inst "2026-03-03")
              cmd-email     {:db/id cmd-email-eid
                             :email/author-address "carol@x"
                             :email/date-sent #inst "2026-03-03"
                             :email/body-text (str "Duplicate-of: " bug-mid "\n")}]
          (commands/apply-commands! conn req-eid :request cmd-email
                                    {} {} :direct nil)
          (let [src-after (d/pull (d/db conn)
                                  [:report/closed :report/close-reason] req-eid)
                rels (get-relations (d/db conn) req-eid)]
            (is (nil? (:report/closed src-after))
                "request not closed when target is a bug (different type)")
            (is (not-any? #(= :duplicates (:kind %)) rels)
                ":duplicates not posted")))
        (finally (close-and-cleanup! setup))))))

(deftest not-duplicate-retracts-relation
  (testing "Not duplicate-of: retracts :duplicates and reopens"
    (let [{:keys [conn] :as setup} (fresh-conn)]
      (try
        (let [orig-mid  "<bug-orig@x>"
              dup-mid   "<bug-dup@x>"
              orig-email (mk-email! conn orig-mid "alice@x" #inst "2026-03-01")
              _orig-eid  (mk-report! conn orig-mid orig-email :bug)
              dup-email  (mk-email! conn dup-mid "bob@x" #inst "2026-03-02")
              dup-eid    (mk-report! conn dup-mid dup-email :bug)
              ;; Carol posts Duplicate-of:
              set-email-eid (mk-email! conn "<set@x>" "carol@x" #inst "2026-03-03")
              set-email {:db/id set-email-eid
                         :email/author-address "carol@x"
                         :email/date-sent #inst "2026-03-03"
                         :email/body-text (str "Duplicate-of: " orig-mid "\n")}
              _ (commands/apply-commands! conn dup-eid :bug set-email {} {} :direct nil)
              ;; Carol retracts (same setter)
              unset-email-eid (mk-email! conn "<unset@x>" "carol@x" #inst "2026-03-04")
              unset-email {:db/id unset-email-eid
                           :email/author-address "carol@x"
                           :email/date-sent #inst "2026-03-04"
                           :email/body-text (str "Not duplicate-of: " orig-mid "\n")}]
          (commands/apply-commands! conn dup-eid :bug unset-email {} {} :direct nil)
          (let [after (d/pull (d/db conn)
                              [:report/closed :report/close-reason] dup-eid)
                active-dup (d/q '[:find [?e ...] :in $ ?from
                                  :where
                                  [?e :rel/from ?from]
                                  [?e :rel/kind :duplicates]
                                  [?e :rel/active? true]]
                                (d/db conn) dup-eid)]
            (is (nil? (:report/closed after))      "duplicate reopened")
            (is (nil? (:report/close-reason after)) "close-reason cleared")
            (is (empty? active-dup)                ":duplicates retracted")))
        (finally (close-and-cleanup! setup))))))

(defn- attach-descendant!
  "Add `email-eid` as descendant of `report-eid`."
  [conn report-eid email-eid]
  (d/transact! conn [[:db/add report-eid :report/descendants email-eid]]))

(deftest directive-resolves-target-via-descendant
  (testing "Related-to: <descendant-mid> resolves to the containing report
            (parity with thread-lookup: a mid that points at any email
            in a report's thread reaches that report)."
    (let [{:keys [conn] :as setup} (fresh-conn)]
      (try
        (let [bug-mid       "<bug-root@x>"
              comment-mid   "<bug-comment@x>"
              other-bug-mid "<other-bug@x>"
              bug-email     (mk-email! conn bug-mid "alice@x" #inst "2026-04-01")
              bug-eid       (mk-report! conn bug-mid bug-email :bug)
              comment-email (mk-email! conn comment-mid "user@x" #inst "2026-04-02")
              _             (attach-descendant! conn bug-eid comment-email)
              other-email   (mk-email! conn other-bug-mid "bob@x" #inst "2026-04-03")
              other-eid     (mk-report! conn other-bug-mid other-email :bug)
              ;; Bob writes Related-to: <bug-comment@x> -- pointing at a
              ;; descendant of bug-eid, not the root.
              cmd-email-eid (mk-email! conn "<cmd@x>" "bob@x" #inst "2026-04-04")
              cmd-email     {:db/id cmd-email-eid
                             :email/author-address "bob@x"
                             :email/date-sent #inst "2026-04-04"
                             :email/body-text (str "Related-to: " comment-mid "\n")}]
          (commands/apply-commands! conn other-eid :bug cmd-email
                                    {} {} :direct nil)
          (let [rels (get-relations (d/db conn) other-eid)
                related (filter #(= :related-to (:kind %)) rels)]
            (is (some #(or (= bug-eid (:to %))
                           (= bug-eid (:from %)))
                      related)
                ":related-to was posted toward bug-eid, even though the
                 cited mid was a descendant of bug-eid, not its root")))
        (finally (close-and-cleanup! setup))))))

;; ---------------------------------------------------------------------------
;; Phase 4: R1-R4 -- patch <-> bug auto-credit and propagation
;; ---------------------------------------------------------------------------

(defn- read-attrs [db eid attrs]
  (d/pull db attrs eid))

(defn- credit-bug!
  "Test helper: directly transact auto-credit state on a bug, mirroring
  what the implicit Acked/Owned mechanism in `apply-commands!` produces
  when a patch email is processed.  Used to set up the preconditions of
  R2/R3/R4 assertions without dragging the full digest pipeline into a
  unit test."
  [conn bug-eid patch-email-eid author-addr]
  (d/transact! conn [{:db/id bug-eid
                      :report/acked patch-email-eid
                      :report/acked-address (str/lower-case author-addr)
                      :report/owned patch-email-eid
                      :report/owned-address (str/lower-case author-addr)}]))

;; R1 (auto-credit on patch creation) is now handled by the implicit
;; Acked/Owned mechanism in apply-commands! and covered by integration
;; tests in digest_test.clj (fixtures 204-205 for the credit path,
;; 206-207 for garde-fou A, 208-209 for the :patch-triggers? gate,
;; 210 for the report-type gate).  The unit tests below focus on
;; R2/R3/R4 (closure propagation, cancel retraction, supersession
;; transfer), which still need explicit setup of the bug's credit
;; state via `credit-bug!`.

(deftest r2-resolved-propagates-to-bug
  (testing "R2: Closed. (=:resolved) on patch closes the bug too"
    (let [{:keys [conn] :as setup} (fresh-conn)]
      (try
        (let [bug-email   (mk-email! conn "<bug@x>" "alice@x" #inst "2026-04-01")
              bug-eid     (mk-report! conn "<bug@x>" bug-email :bug)
              patch-email-map {:db/id (mk-email! conn "<patch@x>" "bob@x" #inst "2026-04-02")
                               :email/author-address "bob@x"
                               :email/date-sent #inst "2026-04-02"
                               :email/source "test"}
              patch-eid   (mk-report! conn "<patch@x>" (:db/id patch-email-map) :patch)
              _ (#'digest/link-rel! conn patch-eid :patch patch-email-map [bug-eid])
              cmd-email-eid (mk-email! conn "<cmd@x>" "bob@x" #inst "2026-04-03")
              cmd-email     {:db/id cmd-email-eid
                             :email/author-address "bob@x"
                             :email/date-sent #inst "2026-04-03"
                             :email/source "test"
                             :email/body-text "Closed.\n"
                             :email/message-id "<cmd@x>"}]
          (commands/apply-commands! conn patch-eid :patch cmd-email
                                    {} {} :direct nil)
          (let [bug-state (read-attrs (d/db conn) bug-eid
                                       [:report/closed :report/close-reason])
                patch-state (read-attrs (d/db conn) patch-eid
                                         [:report/closed :report/close-reason])]
            (is (some? (:report/closed patch-state)) "patch is closed")
            (is (= :resolved (:report/close-reason patch-state)))
            (is (some? (:report/closed bug-state))   "bug auto-closed (R2)")
            (is (= :resolved (:report/close-reason bug-state)))))
        (finally (close-and-cleanup! setup))))))

(deftest r3-cancelled-retracts-auto-credits
  (testing "R3: Cancelled. on auto-creditor patch retracts acked/owned on bug"
    (let [{:keys [conn] :as setup} (fresh-conn)]
      (try
        (let [bug-email   (mk-email! conn "<bug@x>" "alice@x" #inst "2026-04-01")
              bug-eid     (mk-report! conn "<bug@x>" bug-email :bug)
              patch-email-map {:db/id (mk-email! conn "<patch@x>" "bob@x" #inst "2026-04-02")
                               :email/author-address "bob@x"
                               :email/date-sent #inst "2026-04-02"
                               :email/source "test"}
              patch-eid   (mk-report! conn "<patch@x>" (:db/id patch-email-map) :patch)
              _ (#'digest/link-rel! conn patch-eid :patch patch-email-map [bug-eid])
              _ (credit-bug! conn bug-eid (:db/id patch-email-map) "bob@x")
              cmd-email-eid (mk-email! conn "<cmd@x>" "bob@x" #inst "2026-04-03")
              cmd-email     {:db/id cmd-email-eid
                             :email/author-address "bob@x"
                             :email/date-sent #inst "2026-04-03"
                             :email/source "test"
                             :email/body-text "Cancelled.\n"
                             :email/message-id "<cmd@x>"}]
          (commands/apply-commands! conn patch-eid :patch cmd-email
                                    {} {} :direct nil)
          (let [bug-state (read-attrs (d/db conn) bug-eid
                                       [:report/acked :report/owned
                                        :report/acked-address :report/owned-address])]
            (is (nil? (:report/acked bug-state))  "auto-acked retracted")
            (is (nil? (:report/owned bug-state))  "auto-owned retracted")
            (is (nil? (:report/acked-address bug-state)))
            (is (nil? (:report/owned-address bug-state)))))
        (finally (close-and-cleanup! setup))))))

(deftest r3-manual-setter-preserved
  (testing "R3: existing manual setter is NOT touched when patch is cancelled"
    (let [{:keys [conn] :as setup} (fresh-conn)]
      (try
        (let [bug-email   (mk-email! conn "<bug@x>" "alice@x" #inst "2026-04-01")
              bug-eid     (mk-report! conn "<bug@x>" bug-email :bug)
              setter-eid  (mk-email! conn "<setter@x>" "carol@x" #inst "2026-04-01T12:00:00")
              _ (d/transact! conn [{:db/id bug-eid
                                    :report/owned setter-eid
                                    :report/owned-address "carol@x"}])
              patch-email-map {:db/id (mk-email! conn "<patch@x>" "bob@x" #inst "2026-04-02")
                               :email/author-address "bob@x"
                               :email/date-sent #inst "2026-04-02"
                               :email/source "test"}
              patch-eid   (mk-report! conn "<patch@x>" (:db/id patch-email-map) :patch)
              _ (#'digest/link-rel! conn patch-eid :patch patch-email-map [bug-eid])
              ;; Implicit credit would have respected garde-fou A; here we
              ;; simulate it by only crediting acked (owned stays Carol's).
              _ (d/transact! conn [{:db/id bug-eid
                                    :report/acked (:db/id patch-email-map)
                                    :report/acked-address "bob@x"}])
              cmd-email-eid (mk-email! conn "<cmd@x>" "bob@x" #inst "2026-04-03")
              cmd-email     {:db/id cmd-email-eid
                             :email/author-address "bob@x"
                             :email/date-sent #inst "2026-04-03"
                             :email/source "test"
                             :email/body-text "Cancelled.\n"
                             :email/message-id "<cmd@x>"}]
          (commands/apply-commands! conn patch-eid :patch cmd-email
                                    {} {} :direct nil)
          (let [bug-state (read-attrs (d/db conn) bug-eid
                                       [:report/owned-address])]
            (is (= "carol@x" (:report/owned-address bug-state))
                "Carol still owns (manual setter preserved through patch cancel)")))
        (finally (close-and-cleanup! setup))))))

(deftest r4-supersede-transfers-ownership
  (testing "R4: P2 supersedes P1 => auto-ownership transfers to P2's author"
    (let [{:keys [conn] :as setup} (fresh-conn)]
      (try
        (let [bug-email (mk-email! conn "<bug@x>" "alice@x" #inst "2026-04-01")
              bug-eid   (mk-report! conn "<bug@x>" bug-email :bug)
              p1-email-map {:db/id (mk-email! conn "<p1@x>" "bob@x" #inst "2026-04-02")
                            :email/author-address "bob@x"
                            :email/date-sent #inst "2026-04-02"
                            :email/source "test"}
              p1-eid    (mk-report! conn "<p1@x>" (:db/id p1-email-map) :patch)
              _ (#'digest/link-rel! conn p1-eid :patch p1-email-map [bug-eid])
              _ (credit-bug! conn bug-eid (:db/id p1-email-map) "bob@x")
              p2-email-map {:db/id (mk-email! conn "<p2@x>" "dave@x" #inst "2026-04-03")
                            :email/author-address "dave@x"
                            :email/date-sent #inst "2026-04-03"
                            :email/source "test"}
              p2-eid    (mk-report! conn "<p2@x>" (:db/id p2-email-map) :patch)
              ;; P1 is closed via Superseded-by: <p2@x> (sent by anyone)
              cmd-email-eid (mk-email! conn "<cmd@x>" "carol@x" #inst "2026-04-04")
              cmd-email     {:db/id cmd-email-eid
                             :email/author-address "carol@x"
                             :email/date-sent #inst "2026-04-04"
                             :email/source "test"
                             :email/body-text "Superseded-by: <p2@x>\n"
                             :email/message-id "<cmd@x>"}]
          (commands/apply-commands! conn p1-eid :patch cmd-email {} {} :direct nil)
          (let [bug-state (read-attrs (d/db conn) bug-eid
                                       [:report/owned-address :report/acked-address])
                rels-bug (get-relations (d/db conn) bug-eid)]
            (is (= "dave@x" (:report/owned-address bug-state))
                "ownership transferred to P2 author (Dave)")
            (is (= "bob@x" (:report/acked-address bug-state))
                "acked is historical -- stays with the original acker (Bob)")
            (is (some #(and (= :resolves (:kind %)) (= p2-eid (:from %))
                            (= bug-eid (:to %)))
                      rels-bug)
                "P2 inherited the :resolves link to bug")))
        (finally (close-and-cleanup! setup))))))

(deftest r4-chain-of-supersession
  (testing "R4 chain: P1 => P2 => P3, ownership ends up with P3's author"
    (let [{:keys [conn] :as setup} (fresh-conn)]
      (try
        (let [bug-email (mk-email! conn "<bug@x>" "alice@x" #inst "2026-04-01")
              bug-eid   (mk-report! conn "<bug@x>" bug-email :bug)
              p1-email-map {:db/id (mk-email! conn "<p1@x>" "bob@x" #inst "2026-04-02")
                            :email/author-address "bob@x"
                            :email/date-sent #inst "2026-04-02"
                            :email/source "test"}
              p1-eid    (mk-report! conn "<p1@x>" (:db/id p1-email-map) :patch)
              _ (#'digest/link-rel! conn p1-eid :patch p1-email-map [bug-eid])
              _ (credit-bug! conn bug-eid (:db/id p1-email-map) "bob@x")
              p2-email-map {:db/id (mk-email! conn "<p2@x>" "dave@x" #inst "2026-04-03")
                            :email/author-address "dave@x"
                            :email/date-sent #inst "2026-04-03"
                            :email/source "test"}
              p2-eid    (mk-report! conn "<p2@x>" (:db/id p2-email-map) :patch)
              ;; Step 1: P1 superseded by P2 (anyone can do this)
              cmd1-eid (mk-email! conn "<cmd1@x>" "carol@x" #inst "2026-04-04")
              cmd1     {:db/id cmd1-eid :email/author-address "carol@x"
                        :email/date-sent #inst "2026-04-04" :email/source "test"
                        :email/body-text "Superseded-by: <p2@x>\n"
                        :email/message-id "<cmd1@x>"}
              _ (commands/apply-commands! conn p1-eid :patch cmd1 {} {} :direct nil)
              ;; Now P3 arrives
              p3-email-map {:db/id (mk-email! conn "<p3@x>" "eve@x" #inst "2026-04-05")
                            :email/author-address "eve@x"
                            :email/date-sent #inst "2026-04-05"
                            :email/source "test"}
              _p3-eid   (mk-report! conn "<p3@x>" (:db/id p3-email-map) :patch)
              ;; Step 2: P2 superseded by P3
              cmd2-eid (mk-email! conn "<cmd2@x>" "carol@x" #inst "2026-04-06")
              cmd2     {:db/id cmd2-eid :email/author-address "carol@x"
                        :email/date-sent #inst "2026-04-06" :email/source "test"
                        :email/body-text "Superseded-by: <p3@x>\n"
                        :email/message-id "<cmd2@x>"}]
          (commands/apply-commands! conn p2-eid :patch cmd2 {} {} :direct nil)
          (let [bug-state (read-attrs (d/db conn) bug-eid
                                       [:report/owned-address])]
            (is (= "eve@x" (:report/owned-address bug-state))
                "after chain of supersession, ownership is at P3 (Eve)")))
        (finally (close-and-cleanup! setup))))))

(deftest link-rel!-related-to-canonical
  (testing ":related-to between two reports stored canonically (single datom)"
    (let [{:keys [conn] :as setup} (fresh-conn)]
      (try
        (let [a-email   (mk-email! conn "<A@x>" "alice@x" #inst "2026-01-01")
              a-eid     (mk-report! conn "<A@x>" a-email :bug)
              b-email-map {:db/id (mk-email! conn "<B@x>" "bob@x" #inst "2026-01-02")
                           :email/author-address "bob@x"
                           :email/date-sent #inst "2026-01-02"}
              b-eid     (mk-report! conn "<B@x>" (:db/id b-email-map) :bug)
              ;; Both directions of pose should canonicalize to a single datom
              _ (#'digest/link-rel! conn b-eid :bug b-email-map [a-eid])
              ;; Now post the reverse direction with a different "child":
              c-email-map {:db/id (mk-email! conn "<C@x>" "carol@x" #inst "2026-01-03")
                           :email/author-address "carol@x"
                           :email/date-sent #inst "2026-01-03"}
              ;; Re-pose B->A (reverse) explicitly:
              _ (#'digest/link-rel! conn a-eid :bug c-email-map [b-eid])
              rels (d/q '[:find ?id
                          :where [?r :rel/kind :related-to] [?r :rel/id ?id]]
                        (d/db conn))]
          (is (= 1 (count rels))
              "single :related-to datom regardless of pose order"))
        (finally (close-and-cleanup! setup))))))

;; ---------------------------------------------------------------------------
;; Related-to: directive
;; ---------------------------------------------------------------------------

(deftest related-to-directive-posts-relation
  (testing "Related-to: <mid> posts an active :related-to without closing"
    (let [{:keys [conn] :as setup} (fresh-conn)]
      (try
        (let [src-mid    "<a@x>"
              tgt-mid    "<b@x>"
              src-email  (mk-email! conn src-mid "alice@x" #inst "2026-03-01")
              src-eid    (mk-report! conn src-mid src-email :bug)
              tgt-email  (mk-email! conn tgt-mid "bob@x"   #inst "2026-03-02")
              _          (mk-report! conn tgt-mid tgt-email :patch)
              cmd-eid    (mk-email! conn "<cmd@x>" "carol@x" #inst "2026-03-03")
              cmd-email  {:db/id cmd-eid
                          :email/author-address "carol@x"
                          :email/date-sent #inst "2026-03-03"
                          :email/body-text (str "Related-to: " tgt-mid "\n")}]
          (commands/apply-commands! conn src-eid :bug cmd-email
                                    {} {} :direct nil)
          (let [src-after (d/pull (d/db conn)
                                  [:report/closed :report/close-reason] src-eid)
                rels (get-relations (d/db conn) src-eid)
                kinds (set (map :kind rels))]
            (is (nil? (:report/closed src-after))
                "Related-to does not close the source")
            (is (contains? kinds :related-to)
                ":related-to is posted")
            (is (= 1 (count (filter #(= :related-to (:kind %)) rels)))
                "single canonical :related-to datom")))
        (finally (close-and-cleanup! setup))))))

(deftest related-to-directive-cross-type-allowed
  (testing "Related-to: accepts any pair of types (no actionable constraint)"
    (let [{:keys [conn] :as setup} (fresh-conn)]
      (try
        (let [bug-eid    (mk-report! conn "<bug@x>"
                                     (mk-email! conn "<bug@x>" "alice@x" #inst "2026-03-01")
                                     :bug)
              ann-eid    (mk-report! conn "<ann@x>"
                                     (mk-email! conn "<ann@x>" "carol@x" #inst "2026-03-02")
                                     :announcement)
              cmd-eid    (mk-email! conn "<cmd@x>" "bob@x" #inst "2026-03-03")
              cmd-email  {:db/id cmd-eid
                          :email/author-address "bob@x"
                          :email/date-sent #inst "2026-03-03"
                          :email/body-text "Related-to: <ann@x>\n"}]
          (commands/apply-commands! conn bug-eid :bug cmd-email
                                    {} {} :direct nil)
          (let [rels (get-relations (d/db conn) bug-eid)]
            (is (some #(= :related-to (:kind %)) rels)
                "bug<->announcement related-to allowed")
            (is (some? ann-eid))))
        (finally (close-and-cleanup! setup))))))

(deftest related-to-directive-multiple-targets
  (testing "Multiple Related-to: lines in one email post several links"
    (let [{:keys [conn] :as setup} (fresh-conn)]
      (try
        (let [src-eid    (mk-report! conn "<src@x>"
                                     (mk-email! conn "<src@x>" "alice@x" #inst "2026-03-01")
                                     :bug)
              _          (mk-report! conn "<a@x>"
                                     (mk-email! conn "<a@x>" "bob@x" #inst "2026-03-02")
                                     :bug)
              _          (mk-report! conn "<b@x>"
                                     (mk-email! conn "<b@x>" "bob@x" #inst "2026-03-02")
                                     :bug)
              cmd-eid    (mk-email! conn "<cmd@x>" "carol@x" #inst "2026-03-03")
              cmd-email  {:db/id cmd-eid
                          :email/author-address "carol@x"
                          :email/date-sent #inst "2026-03-03"
                          :email/body-text "Related-to: <a@x>\nRelated-to: <b@x>\n"}]
          (commands/apply-commands! conn src-eid :bug cmd-email
                                    {} {} :direct nil)
          (let [rels (filter #(= :related-to (:kind %))
                             (get-relations (d/db conn) src-eid))]
            (is (= 2 (count rels))
                "two distinct :related-to datoms posted")))
        (finally (close-and-cleanup! setup))))))

(deftest not-related-to-directive-retracts
  (testing "Not related-to: <mid> retracts the active :related-to"
    (let [{:keys [conn] :as setup} (fresh-conn)]
      (try
        (let [src-eid    (mk-report! conn "<src@x>"
                                     (mk-email! conn "<src@x>" "alice@x" #inst "2026-03-01")
                                     :bug)
              tgt-mid    "<tgt@x>"
              _          (mk-report! conn tgt-mid
                                     (mk-email! conn tgt-mid "bob@x" #inst "2026-03-02")
                                     :bug)
              ;; Pose first via the directive itself
              pose-eid   (mk-email! conn "<pose@x>" "carol@x" #inst "2026-03-03")
              pose-email {:db/id pose-eid
                          :email/author-address "carol@x"
                          :email/date-sent #inst "2026-03-03"
                          :email/body-text (str "Related-to: " tgt-mid "\n")}
              _          (commands/apply-commands! conn src-eid :bug pose-email
                                                    {} {} :direct nil)
              ;; Then retract via Not related-to:
              ret-eid    (mk-email! conn "<ret@x>" "carol@x" #inst "2026-03-04")
              ret-email  {:db/id ret-eid
                          :email/author-address "carol@x"
                          :email/date-sent #inst "2026-03-04"
                          :email/body-text (str "Not related-to: " tgt-mid "\n")}]
          (commands/apply-commands! conn src-eid :bug ret-email
                                    {} {} :direct nil)
          (let [rels (filter #(= :related-to (:kind %))
                             (get-relations (d/db conn) src-eid))]
            (is (every? (complement :active?) rels)
                "all :related-to relations are retracted")))
        (finally (close-and-cleanup! setup))))))

(deftest related-to-directive-on-closed-report
  (testing "Related-to: works on a closed report (additive, no closure side-effect)"
    (let [{:keys [conn] :as setup} (fresh-conn)]
      (try
        (let [src-mid    "<src@x>"
              src-eid    (mk-report! conn src-mid
                                     (mk-email! conn src-mid "alice@x" #inst "2026-03-01")
                                     :bug)
              ;; Close the source first
              _          (d/transact! conn [{:db/id src-eid
                                              :report/closed (mk-email! conn "<close@x>"
                                                                         "alice@x"
                                                                         #inst "2026-03-02")
                                              :report/close-reason :resolved}])
              tgt-mid    "<tgt@x>"
              _          (mk-report! conn tgt-mid
                                     (mk-email! conn tgt-mid "bob@x" #inst "2026-03-03")
                                     :bug)
              cmd-eid    (mk-email! conn "<cmd@x>" "carol@x" #inst "2026-03-04")
              cmd-email  {:db/id cmd-eid
                          :email/author-address "carol@x"
                          :email/date-sent #inst "2026-03-04"
                          :email/body-text (str "Related-to: " tgt-mid "\n")}]
          (commands/apply-commands! conn src-eid :bug cmd-email
                                    {} {} :direct nil)
          (let [src-after (d/pull (d/db conn)
                                  [:report/closed :report/close-reason] src-eid)
                rels (filter #(= :related-to (:kind %))
                             (get-relations (d/db conn) src-eid))]
            (is (some? (:report/closed src-after))
                "source remains closed")
            (is (= :resolved (:report/close-reason src-after))
                "close-reason unchanged")
            (is (some :active? rels)
                ":related-to is posted even on a closed report")))
        (finally (close-and-cleanup! setup))))))

(deftest pose-if-absent-reactivates-after-retract
  (testing "re-posing a retracted relation reactivates it iff dated after the retraction"
    (let [{:keys [conn] :as setup} (fresh-conn)]
      (try
        (let [e1 (mk-email! conn "<pose@x>"    "alice@x" #inst "2026-01-01")
              e2 (mk-email! conn "<retract@x>" "alice@x" #inst "2026-01-02")
              e3 (mk-email! conn "<repose@x>"  "bob@x"   #inst "2026-01-03")
              r1 (mk-report! conn "<pose@x>"    e1 :patch)
              r2 (mk-report! conn "<repose@x>"  e3 :patch)
              base {:from-eid r1 :to-eid r2 :kind :supersedes :value nil}]
          (is (some? (rel/pose-if-absent!
                      conn (assoc base :setter "alice@x" :email-eid e1
                                  :posed-at #inst "2026-01-01")))
              "initial pose transacts")
          (is (nil? (rel/pose-if-absent!
                     conn (assoc base :setter "alice@x" :email-eid e1
                                 :posed-at #inst "2026-01-01")))
              "re-pose of an active relation is a no-op")
          (is (true? (rel/retract-pair! conn r1 :supersedes r2 e2))
              "retraction (by the email dated 2026-01-02)")
          (is (nil? (rel/pose-if-absent!
                     conn (assoc base :setter "alice@x" :email-eid e1
                                 :posed-at #inst "2026-01-01")))
              "a pose dated before the retraction does NOT reactivate (replay safety)")
          (is (every? (complement :active?)
                      (get-relations (d/db conn) r1))
              "relation still retracted after the out-of-order pose")
          (is (some? (rel/pose-if-absent!
                      conn (assoc base :setter "BOB@x" :email-eid e3
                                  :posed-at #inst "2026-01-03")))
              "a pose dated after the retraction reactivates")
          (let [rels (filter #(= :supersedes (:kind %))
                             (get-relations (d/db conn) r1))]
            (is (seq rels))
            (is (every? :active? (get-relations (d/db conn) r1))
                "both directions active again")
            (is (every? #(= "bob@x" (:setter %))
                        (get-relations (d/db conn) r1))
                "the re-poser becomes the setter (lowercased)")))
        (finally (close-and-cleanup! setup))))))
