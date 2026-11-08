(ns bone.periods-test
  (:require [clojure.test :refer [are deftest is testing]]
            [bone.common :as common]
            [bone.periods :as p]))

(defn- d [s] (common/parse-iso-date s))

(deftest source-periods-test
  (testing "no :periods: a single all-time era carrying the source's values"
    (let [src {:name "s" :maintainers ["a@x.org"] :commands {:closed {:words ["Done"]}}}
          [only] (p/source-periods src)]
      (is (nil? (:from only)))
      (is (nil? (:to only)))
      (is (= ["a@x.org"] (:maintainers only)))
      (is (= {:closed {:words ["Done"]}} (:commands only)))))
  (testing "an era inherits what it does not declare from the source"
    (let [src {:maintainers ["default@x.org"]
               :commands    {:closed {:words ["Closed"]}}
               :periods     [{:end "2020-01-01"
                              :maintainers ["old@x.org"]}
                             {:start "2020-01-01"}]}
          [era1 era2] (p/source-periods src)]
      (is (= ["old@x.org"] (:maintainers era1)) "era1 overrides :maintainers")
      (is (= {:closed {:words ["Closed"]}} (:commands era1)) "era1 inherits :commands")
      (is (= ["default@x.org"] (:maintainers era2)) "era2 inherits both")
      (is (= {:closed {:words ["Closed"]}} (:commands era2)))))
  (testing "ISO bounds are parsed"
    (let [[era] (p/source-periods {:periods [{:start "2020-01-01" :end "2024-01-01"}]})]
      (is (= (d "2020-01-01") (:from era)))
      (is (= (d "2024-01-01") (:to era))))))

(deftest period-at-date-test
  (let [periods [{:from nil :to (d "2020-01-01") :tag :a}
                 {:from (d "2020-01-01") :to (d "2024-01-01") :tag :b}
                 {:from (d "2024-01-01") :to nil :tag :c}]]
    (testing "half-open windows: :from inclusive, :to exclusive, unbounded ends"
      (are [date tag] (= tag (:tag (p/period-at-date periods (d date))))
        "1999-05-05" :a
        "2020-01-01" :b
        "2023-12-31" :b
        "2024-01-01" :c
        "2030-06-01" :c)))
  (testing "nil date, or a date in a gap, finds no period"
    (is (nil? (p/period-at-date [{:from nil :to nil}] nil)))
    (is (nil? (p/period-at-date [{:from (d "2020-01-01") :to (d "2022-01-01")}
                                 {:from (d "2023-01-01") :to nil}]
                                (d "2022-06-01"))))))

(deftest source-cfg-at-date-test
  (let [src {:name        "s"
             :list        "x@list"
             :maintainers ["current@x.org"]
             :commands    {:closed {:words ["Closed"]}}
             :periods     [{:end "2020-01-01"
                            :maintainers ["old@x.org"]
                            :commands    {:closed {:words ["Done"]}}}
                           {:start "2020-01-01"}]}]
    (testing "a date in an era takes the era's overrides, keeps the rest"
      (let [cfg (p/source-cfg-at-date src (d "2019-06-01"))]
        (is (= ["old@x.org"] (:maintainers cfg)))
        (is (= {:closed {:words ["Done"]}} (:commands cfg)))
        (is (= "x@list" (:list cfg)) "non-overridable fields preserved")))
    (testing "an era declaring nothing inherits from the source"
      (let [cfg (p/source-cfg-at-date src (d "2025-01-01"))]
        (is (= ["current@x.org"] (:maintainers cfg)))
        (is (= {:closed {:words ["Closed"]}} (:commands cfg))))))
  (testing "no :periods: identity on the overridable fields"
    (is (= ["a@x.org"]
           (:maintainers (p/source-cfg-at-date {:name "s" :maintainers ["a@x.org"] :commands {}}
                                               (d "2024-01-01"))))))
  (testing "a date outside every era falls back to the source level, not the nearest era"
    (is (= ["src@x"]
           (:maintainers (p/source-cfg-at-date
                          {:maintainers ["src@x"]
                           :periods     [{:end "2020-01-01" :maintainers ["a@x"]}
                                         {:start "2023-01-01" :maintainers ["b@x"]}]}
                          (d "2022-01-01")))))
    (let [cfg (p/source-cfg-at-date
               {:maintainers ["src@x"]
                :commands    {:closed {:words ["Closed"]}}
                :periods     [{:start "2020-01-01" :end "2026-04-18"
                               :commands {:closed {:words ["Done"]}}}]}
               (d "2026-05-13"))]
      (is (= ["src@x"] (:maintainers cfg)))
      (is (= {:closed {:words ["Closed"]}} (:commands cfg))
          "expired period's overrides do not bleed past its :to")))
  (testing ":patch-triggers? is overridable per era"
    (is (false? (:patch-triggers?
                 (p/source-cfg-at-date {:patch-triggers? true
                                        :periods [{:start "2026-01-01" :end "2027-01-01"
                                                   :patch-triggers? false}]}
                                       (d "2026-06-01")))))))

(deftest validate-periods-test
  (testing "well-formed"
    (is (= [] (p/validate-periods {:name "s" :maintainers ["a@x"]})))
    (is (= [] (p/validate-periods
               {:periods [{:end "2020-01-01" :maintainers ["a@x"]}
                          {:start "2020-01-01" :end "2024-01-01" :maintainers ["b@x"]}
                          {:start "2024-01-01" :maintainers ["c@x"]}]}))))
  (testing "each malformation names its error"
    (are [periods re] (some #(re-find re %) (p/validate-periods {:periods periods}))
      [{:start "01/01/2020"}]                          #":start not ISO"
      [{:start "2026-02-31"}]                          #":start not ISO"
      [{:start "2022-01-01" :end "2020-01-01"}]        #":start must be strictly before :end"
      [{:end "2020-01-01"} {:start "2021-01-01"}]      #"gap/overlap"
      [{:end "2021-01-01"} {:start "2020-06-01"}]      #"gap/overlap"
      [{} {:start "2020-01-01"}]                       #"missing :end"
      [{:end "2020-01-01"} {}]                         #"missing :start"
      ["oops"]                                         #"not a map")))
