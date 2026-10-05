;; Copyright (c) 2026 Bastien Guerry <bzg@gnu.org>
;; SPDX-License-Identifier: EPL-2.0
;; License-Filename: LICENSES/EPL-2.0.txt

(ns bone.relations-test
  (:require [clojure.test :refer [deftest is testing]]
            [bone.relations :as r]))

(deftest make-relation-id-test
  (is (= "10:resolves:20"     (r/make-relation-id 10 :resolves 20)))
  (is (= "20:resolved-by:10"  (r/make-relation-id 20 :resolved-by 10))))

(deftest inverse-kinds-test
  (is (= :resolved-by (r/inverse-kinds :resolves)))
  (is (= :resolves    (r/inverse-kinds :resolved-by)))
  (is (= :related-to  (r/inverse-kinds :related-to))
      "symmetric kind is its own inverse"))

(deftest canonicalize-test
  (testing "sorts ascending by eid"
    (is (= [10 20] (r/canonicalize 10 20)))
    (is (= [10 20] (r/canonicalize 20 10)))))

(deftest valid-pose?-test
  (testing ":duplicates requires same type, both actionable"
    (is (true?  (r/valid-pose? :duplicates 1 2 :bug :bug)))
    (is (false? (r/valid-pose? :duplicates 1 2 :bug :patch))
        "different types rejected")
    (is (false? (r/valid-pose? :duplicates 1 2 :ann :ann))
        "non-actionable type rejected")
    (is (true?  (r/valid-pose? :duplicates 1 2 :patch :patch)))
    (is (true?  (r/valid-pose? :duplicates 1 2 :request :request))))
  (testing ":supersedes requires same type, both actionable"
    (is (true?  (r/valid-pose? :supersedes 1 2 :patch :patch)))
    (is (false? (r/valid-pose? :supersedes 1 2 :patch :bug))))
  (testing ":resolves requires patch source and bug/request target"
    (is (true?  (r/valid-pose? :resolves 1 2 :patch :bug)))
    (is (true?  (r/valid-pose? :resolves 1 2 :patch :request)))
    (is (false? (r/valid-pose? :resolves 1 2 :patch :ann))
        "non-actionable target rejected")
    (is (false? (r/valid-pose? :resolves 1 2 :patch :patch))
        "patch->patch rejected (would auto-credit cover letter from its own series)")
    (is (false? (r/valid-pose? :resolves 1 2 :bug :patch))
        "bug source rejected (use :resolved-by for the reverse direction)")
    (is (true?  (r/valid-pose? :resolved-by 1 2 :bug :patch)))
    (is (true?  (r/valid-pose? :resolved-by 1 2 :request :patch)))
    (is (false? (r/valid-pose? :resolved-by 1 2 :patch :patch))
        "patch->patch rejected on the inverse direction too"))
  (testing ":related-to accepts any type"
    (is (true?  (r/valid-pose? :related-to 1 2 :ann :release)))
    (is (true?  (r/valid-pose? :related-to 1 2 :bug :patch))))
  (testing "no self-loops"
    (is (false? (r/valid-pose? :resolves 1 1 :patch :bug)))
    (is (false? (r/valid-pose? :related-to 1 1 :ann :ann))))
  (testing "unknown kind"
    (is (false? (r/valid-pose? :unknown 1 2 :bug :bug)))))

(deftest pose-tx-asymmetric-test
  (let [tx (r/pose-tx {:from-eid 10 :to-eid 20 :kind :resolves
                       :setter "BOB@example.com" :email-eid 99
                       :posed-at #inst "2026-05-09" :value nil})]
    (is (= 2 (count tx)) "two datoms for asymmetric kind")
    (is (= "10:resolves:20"    (:rel/id (first tx))))
    (is (= "20:resolved-by:10" (:rel/id (second tx))))
    (is (= "bob@example.com" (:rel/setter (first tx)))
        "setter lowercased")
    (is (every? :rel/active? tx))
    (is (not (contains? (first tx) :rel/value))
        "nil value not stored")))

(deftest pose-tx-symmetric-test
  (let [tx-ab (r/pose-tx {:from-eid 10 :to-eid 20 :kind :related-to
                          :setter "alice@x" :email-eid 99
                          :posed-at #inst "2026-05-09" :value nil})
        tx-ba (r/pose-tx {:from-eid 20 :to-eid 10 :kind :related-to
                          :setter "alice@x" :email-eid 99
                          :posed-at #inst "2026-05-09" :value nil})]
    (is (= 1 (count tx-ab)) "one datom for symmetric kind")
    (is (= (:rel/id (first tx-ab)) (:rel/id (first tx-ba)))
        "A->B and B->A produce same canonical :rel/id")))

(deftest pose-tx-with-value-test
  (let [tx (r/pose-tx {:from-eid 10 :to-eid 20 :kind :supersedes
                       :setter "x@y" :email-eid 99
                       :posed-at #inst "2026-05-09"
                       :value "<target@host>"})]
    (is (= "<target@host>" (:rel/value (first tx))))
    (is (= "<target@host>" (:rel/value (second tx)))
        "value carried on inverse direction too (audit)")))

(deftest paired-relation-ids-test
  (is (= ["10:resolves:20" "20:resolved-by:10"]
         (r/paired-relation-ids :resolves 10 20)))
  (is (= ["10:related-to:20"]
         (r/paired-relation-ids :related-to 10 20)))
  (is (= ["10:related-to:20"]
         (r/paired-relation-ids :related-to 20 10))
      "symmetric: same canonical id regardless of caller order"))

(deftest retract-tx-test
  (let [tx (r/retract-tx 42 99)]
    (is (= 1 (count tx)))
    (is (= 42 (:db/id (first tx))))
    (is (false? (:rel/active? (first tx))))
    (is (= 99 (:rel/retracted-by (first tx))))))

(deftest retract-by-direction-asymmetric-only-test
  ;; Symmetric kinds are canonicalized by ascending eid order, so a
  ;; one-sided :rel/from (or :rel/to) scan would miss half of them --
  ;; the guard fires before any DB access.
  (is (thrown? AssertionError (r/retract-by-from! nil 1 :related-to 99))))
