(ns bone.config-test
  "Unit tests for bone.config: build-source-map edge cases."
  (:require [clojure.test :refer [deftest is testing]]
            [bone.config :as config]))

;; ---------------------------------------------------------------------------
;; build-source-map -- nil source-type exclusion (fix 11)
;; ---------------------------------------------------------------------------

(deftest build-source-map-excludes-nil-type
  (let [config {:sources [{:name "good" :list "bugs.example.org"}
                           {:name "bad"}]}
        sm     (config/build-source-map config)]
    (is (contains? sm "good"))
    (is (not (contains? sm "bad")))))

(deftest build-source-map-propagates-runtime-options
  (let [config {:restricted-types #{:bug}
                :patch-triggers? false
                :sources [{:name "global" :list "global.example.org"}
                          {:name "source" :list "source.example.org"
                           :restricted-types #{}
                           :patch-triggers? true}]}
        sm     (config/build-source-map config)]
    (testing "global values apply when a source does not override them"
      (is (= #{:bug} (get-in sm ["global" :restricted-types])))
      (is (false? (get-in sm ["global" :patch-triggers?]))))
    (testing "source values override globals, including empty/falsey values"
      (is (= #{} (get-in sm ["source" :restricted-types])))
      (is (true? (get-in sm ["source" :patch-triggers?]))))))

(deftest build-source-map-keeps-project-links
  (testing "the project-link keys survive the source-map whitelist --
            they feed the JSON envelope and the exported pages"
    (let [sm (config/build-source-map
              {:sources [{:name "s" :list "l.example.org"
                          :website        "https://p.example.org"
                          :contribute-url "https://p.example.org/contribute"
                          :post-address   "bugs@example.org"}]})]
      (is (= "https://p.example.org" (get-in sm ["s" :website])))
      (is (= "https://p.example.org/contribute" (get-in sm ["s" :contribute-url])))
      (is (= "bugs@example.org" (get-in sm ["s" :post-address]))))))

(deftest db-path-expands-home
  (testing "a leading ~/ in :db :path is expanded, so the daemon and
            the bb scripts open the same DB"
    (is (= (str (System/getProperty "user.home") "/bone/db")
           (config/db-path {:db {:path "~/bone/db"}})))))
