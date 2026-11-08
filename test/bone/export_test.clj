(ns bone.export-test
  "Unit tests for bone.export, the pure helpers of the bb scripts:
  CLI parsing, report scoring, votes, raw message-ids, the
  reproducible per-source config."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure.edn :as edn]
            [bone.export :as export]))

(deftest extract-bracketed-id-raw-test
  (testing "preserves original case of the id-right"
    (is (= "<abc@Example.COM>" (export/extract-bracketed-id-raw "<abc@Example.COM>")))
    (is (= "<Local-Part@EXAMPLE.com>"
           (export/extract-bracketed-id-raw "<Local-Part@EXAMPLE.com>"))))
  (testing "first bracketed token wins, padding trimmed, vector accepted"
    (is (= "<abc@X>" (export/extract-bracketed-id-raw "<abc@X> (c) <other@Y>")))
    (is (= "<abc@X>" (export/extract-bracketed-id-raw "   <abc@X>   ")))
    (is (= "<abc@X>" (export/extract-bracketed-id-raw ["<abc@X>" "<other@Y>"]))))
  (testing "rejects malformed input identically to extract-bracketed-id"
    (is (nil? (export/extract-bracketed-id-raw "no brackets")))
    (is (nil? (export/extract-bracketed-id-raw "<foo bar@X>")))
    (is (nil? (export/extract-bracketed-id-raw nil)))
    (is (nil? (export/extract-bracketed-id-raw "")))
    (is (nil? (export/extract-bracketed-id-raw "   ")))))

(deftest effective-source-config-test
  (let [config {:labels   {:bug ["BUG" "DEFECT"]}
                :commands {:closed {:words ["Done"]}}
                :notifications {:smtp {:password "secret"}
                                :subscribers [{:email "subscriber@example.org"}]}
                :sources  [{:name          "demo"
                            :list          "demo.example.org"
                            :to            "demo@example.org"
                            :base-url      "https://bone.example.org/public/demo/"
                            :maintainers   ["lead@example.org"]
                            :command-syntax :strict
                            :labels        {:patch ["PATCH"]}
                            :commands      {:closed {:scope :maintainer}}
                            :notifications {:enabled false}}]}
        eff    (export/effective-source-config config "demo")]
    (testing "exactly one matcher is emitted, by source-type precedence"
      (is (= "demo.example.org" (:list eff)))
      (is (not (contains? eff :to))))
    (testing "links and maintainers are kept"
      (is (= "https://bone.example.org/public/demo/" (:base-url eff)))
      (is (= ["lead@example.org"] (:maintainers eff))))
    (testing "global :labels fold in, per-source :labels merge over them"
      (is (= {:bug ["BUG" "DEFECT"] :patch ["PATCH"]} (:labels eff))))
    (testing "global :words and per-source overrides combine per command"
      (is (= {:closed {:words ["Done"] :scope :maintainer}} (:commands eff))))
    (testing ":command-syntax surfaces only when :strict"
      (is (= :strict (:command-syntax eff))))
    (testing "secrets and PII never leak"
      (is (not (contains? eff :notifications)))
      (is (nil? (some #{"secret"} (tree-seq coll? seq eff)))))
    (testing "unknown source => nil"
      (is (nil? (export/effective-source-config config "missing"))))))

(deftest effective-source-config-flat-command-override-test
  ;; The daemon replaces a command's {:scope :report-types} override
  ;; unit wholesale when the source redefines it (see
  ;; common/resolve-command-overrides): the published effective config
  ;; must not deep-merge the global :report-types back in.
  (let [config {:commands {:closed {:scope :maintainer :report-types [:bug]}}
                :sources  [{:name "demo" :list "demo.example.org"
                            :commands {:closed {:scope :anyone}}}]}
        eff    (export/effective-source-config config "demo")]
    (is (= {:closed {:scope :anyone}} (:commands eff))
        "the local override unit fully replaces the global one")))

(deftest reproducible-config-test
  (let [config {:sources [{:name "demo" :list "demo.example.org"}]}
        data   (export/reproducible-config config "demo")
        s      (export/reproducible-config-str config "demo")]
    (testing ":mailboxes is a single Maildir placeholder"
      (is (= 1 (count (:mailboxes data))))
      (is (= :maildir (:type (first (:mailboxes data))))))
    (testing ":sources carries the effective source"
      (is (= "demo.example.org" (:list (first (:sources data))))))
    (testing "string form is commented and parses back as the same data"
      (is (str/starts-with? s ";;"))
      (is (= data (edn/read-string s))))))

(deftest parse-cli-args-test
  (testing "a leading non-flag token is the format"
    (is (= "root" (:format (export/parse-cli-args ["root" "--force"]))))
    (is (= "root" (:format (export/parse-cli-args ["--index-only"]))))
    (is (nil? (:format (export/parse-cli-args ["--force"])))))
  (testing "valued flags"
    (is (= "event,security"
           (:topics-filter (export/parse-cli-args ["--topics-filter" "event,security"]))))
    (is (= 2 (:min-priority (export/parse-cli-args ["-p" "2"]))))
    (is (nil? (:topics-filter (export/parse-cli-args ["--force"])))))
  (testing "a valued flag followed by a flag keeps no value"
    (is (nil? (:source-name (export/parse-cli-args ["-n" "--force"]))))))

(deftest report-status-test
  (is (= 7 (export/report-status {:report/owned 1 :report/acked 1})))
  (is (= 0 (export/report-status {:report/closed 1})))
  (is (= 2 (export/report-descendant-count {:report/descendants [1 2]})))
  (is (= 0 (export/report-descendant-count {}))))

(deftest vote-counts-test
  (is (= {:up 2 :down 0 :null 1}
         (export/vote-counts [{:value :up} {:value :null} {:value :up}])))
  (is (= {:up 0 :down 0 :null 0} (export/vote-counts [])))
  (testing "votes-by-report keeps the raw mid case from the headers"
    (let [votes (export/votes-by-report
                 [[1 :up "a@x" "<id@x>" "{\"Message-Id\" [\"<ID@x>\"]}"]
                  [1 :down "b@x" "<other@x>" nil]])]
      (is (= ["<ID@x>" "<other@x>"] (mapv :email-mid-raw (get votes 1)))))))
