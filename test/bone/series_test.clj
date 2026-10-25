(ns bone.series-test
  "Unit tests for the pure decision functions of bone.series:
  id allocation (next-sid) and implicit series restarts
  (series-restart-plan).  The effectful paths are covered by the
  integration tests in bone.digest-test."
  (:require [clojure.test :refer [deftest is testing]]
            [bone.series :as series]))

(deftest next-sid-allocation
  (testing "base is free"
    (is (= "t|a@x|2" (series/next-sid "t|a@x|2" []))))
  (testing "base taken counts as suffix 1"
    (is (= "t|a@x|2#2" (series/next-sid "t|a@x|2" ["t|a@x|2"]))))
  (testing "next id is one past the highest suffix, holes included"
    (is (= "t|a@x|2#4" (series/next-sid "t|a@x|2" ["t|a@x|2" "t|a@x|2#3"]))))
  (testing "longer totals sharing the prefix do not count"
    (is (= "t|a@x|2" (series/next-sid "t|a@x|2" ["t|a@x|25" "t|a@x|25#3"])))))

(deftest series-restart-plan-decisions
  (let [s1 {:eid 1 :seqs #{"1/2" "2/2"} :mids #{"<a>" "<b>"}
            :cover-mid "<c0>" :empty? false}]
    (testing "a new cover (0/N) threading back to the old series closes it"
      (is (= [1] (series/series-restart-plan 0 [s1] #{"<a>"}))))
    (testing "a new 1/N closes a series that already holds a 1/..."
      (is (= [1] (series/series-restart-plan 1 [s1] #{"<b>"}))))
    (testing "threading back to the old cover letter is enough"
      (is (= [1] (series/series-restart-plan 0 [s1] #{"<c0>"}))))
    (testing "a zero-padded 01/N (git format-patch, ten patches or more)
              counts as a first patch"
      (let [padded {:eid 3 :seqs #{"01/12" "02/12"} :mids #{"<p>"}
                    :cover-mid "<c3>" :empty? false}]
        (is (= [3] (series/series-restart-plan 1 [padded] #{"<p>"})))))
    (testing "a 2/N alone never restarts"
      (is (nil? (series/series-restart-plan 2 [s1] #{"<a>"}))))
    (testing "no thread link to the old series, no restart"
      (is (nil? (series/series-restart-plan 1 [s1] #{"<z>"}))))
    (testing "no parents at all, no restart"
      (is (nil? (series/series-restart-plan 1 [s1] #{}))))
    (testing "no existing series, no restart"
      (is (nil? (series/series-restart-plan 0 [] #{"<a>"}))))
    (testing "a numbered patch spares the empty series its own cover opened"
      (let [awaiting {:eid 2 :seqs #{} :mids #{}
                      :cover-mid "<c2>" :empty? true}]
        (is (= [1] (series/series-restart-plan
                    1 [s1 awaiting] #{"<b>" "<c2>"})))))
    (testing "a cover (0/N) closes even an empty series"
      (let [awaiting {:eid 2 :seqs #{} :mids #{}
                      :cover-mid "<c2>" :empty? true}]
        (is (= [1 2] (series/series-restart-plan
                      0 [s1 awaiting] #{"<a>" "<c2>"})))))))
