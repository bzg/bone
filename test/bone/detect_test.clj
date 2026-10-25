(ns bone.detect-test
  "Unit tests for bone.detect."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [bone.detect :as detect]))

;; ---------------------------------------------------------------------------
;; Strict label regex: case-sensitive tag, mandatory `\s` or EOL after `]`
;; ---------------------------------------------------------------------------

(defn- detect-type [subject]
  (:type (detect/detect-report {:email/subject subject})))

(deftest label-regex-strict-test
  (testing "accepted forms"
    (is (= :bug   (detect-type "[BUG] Mon bug")))
    (is (= :bug   (detect-type "[BUG foo] Mon bug")))
    (is (= :bug   (detect-type "[BUG]")))
    (is (= :bug   (detect-type "[mylist] [BUG] Mon bug")))
    (is (= :patch (detect-type "[PATCH foo v2 1/2] Body")))
    (is (= :request     (detect-type "[POLL] question?")))
    (is (= :request     (detect-type "[TODO] task")))
    (is (= :announcement (detect-type "[ANN] news")))
    (is (= :announcement (detect-type "[ANNOUNCEMENT] news")))
    (is (= :release  (detect-type "[REL 2.0] notes")))
    (is (= :change   (detect-type "[CHG 9.8] heads-up"))))

  (testing "rejected: wrong case"
    (is (nil? (detect-type "[Bug] Mon bug")))
    (is (nil? (detect-type "[bug] Mon bug")))
    (is (nil? (detect-type "[Patch] body")))
    (is (nil? (detect-type "[poll] question?"))))

  (testing "rejected: no whitespace after closing bracket"
    (is (nil? (detect-type "[BUG]Mon bug")))
    (is (nil? (detect-type "[BUG foo]bar")))
    (is (nil? (detect-type "[POLL]?"))))

  (testing "rejected: malformed bracket content"
    (is (nil? (detect-type "[BUG/RFC] Mon bug")))
    (is (nil? (detect-type "[ BUG ] Mon bug")))
    (is (nil? (detect-type "BUG: Mon bug")))
    (is (nil? (detect-type "Re: [BUG] Mon bug")))))

;; ---------------------------------------------------------------------------
;; Label-decides priority: subject label wins over patch content
;; ---------------------------------------------------------------------------

(def ^:private patch-attachment
  [{:attachment/filename "fix.patch"
    :attachment/content-type "text/x-patch"
    :attachment/size 1234}])

(def ^:private inline-diff-body
  (str "diff --git a/foo.el b/foo.el\n"
       "index abc123..def456 100644\n"
       "--- a/foo.el\n+++ b/foo.el\n@@ -1,3 +1,4 @@\n+;; fix\n"))

(deftest label-wins-over-patch-content-test
  (testing "[BUG] + .patch attachment yields :bug, not :patch"
    (is (= :bug (:type (detect/detect-report
                        {:email/subject    "[BUG] something broken"
                         :email/attachments patch-attachment})))))

  (testing "[POLL] + .patch attachment yields :request"
    (is (= :request (:type (detect/detect-report
                            {:email/subject    "[POLL] new feature?"
                             :email/attachments patch-attachment})))))

  (testing "[PATCH] subject still yields :patch (even without attachment)"
    (is (= :patch (:type (detect/detect-report
                          {:email/subject "[PATCH] subject-only patch"})))))

  (testing "inline diff alone in the body is no longer a patch signal"
    (is (nil? (detect/detect-report
               {:email/subject   "Small cleanup"
                :email/body-text inline-diff-body})))))

(deftest re-prefix-patch-with-attachment-test
  (testing "Re: [PATCH] foo WITHOUT attachment yields nil"
    (is (nil? (detect-type "Re: [PATCH] feature"))))

  (testing "Re: [PATCH] foo WITH attachment matches :patch (v2/v3 reply workflow)"
    (is (= :patch (:type (detect/detect-report
                          {:email/subject     "Re: [PATCH] feature"
                           :email/in-reply-to "<parent@test.org>"
                           :email/attachments patch-attachment})))))

  (testing "Re: [PATCH v2] with attachment keeps version handling"
    (is (= "v2" (:version (detect/detect-report
                           {:email/subject     "Re: [PATCH v2] feature"
                            :email/in-reply-to "<parent@test.org>"
                            :email/attachments patch-attachment})))))

  (testing "Re: [BUG] never matches :bug (strict for non-patch)"
    (is (nil? (:type (detect/detect-report
                      {:email/subject     "Re: [BUG] something"
                       :email/in-reply-to "<parent@test.org>"
                       :email/attachments patch-attachment}))))))

(def ^:private format-patch-attachment
  "A .patch attachment that looks like real `git format-patch` output."
  [{:attachment/filename "0001-fix-it.patch"
    :attachment/content-type "text/x-patch"
    :attachment/data (str "From 0123456789abcdef0123456789abcdef01234567 Mon Sep 17 00:00:00 2026\n"
                          "From: Alice <alice@test.org>\n"
                          "Date: Mon, 11 May 2026 09:00:00 +0000\n"
                          "Subject: [PATCH] Fix it\n\n"
                          "diff --git a/foo b/foo\n--- a/foo\n+++ b/foo\n@@ -1,1 +1,1 @@\n-old\n+new\n")}])

(deftest format-patch-attachment-escapes-thread-test
  (testing "Re: <no PATCH label> + real git-format-patch attachment -> :patch"
    (is (= :patch (:type (detect/detect-report
                          {:email/subject     "Re: Add font lock for structures"
                           :email/in-reply-to "<parent@test.org>"
                           :email/attachments format-patch-attachment})))))

  (testing "Re: <no label> + format-patch attachment without [PATCH] in inner Subject -> nil"
    (let [att-no-patch [{:attachment/filename "0001-demo.patch"
                         :attachment/content-type "text/x-patch"
                         :attachment/data (str "From 0123456789abcdef0123456789abcdef01234567 Mon Sep 17 00:00:00 2026\n"
                                               "From: Alice <alice@test.org>\n"
                                               "Subject: [DEMO] Just a demo\n\n"
                                               "diff --git a/foo b/foo\n")}]]
      (is (nil? (detect/detect-report
                 {:email/subject     "Re: Something"
                  :email/in-reply-to "<parent@test.org>"
                  :email/attachments att-no-patch})))))

  (testing "[BUG] subject + real format-patch attachment -> :bug (label still wins)"
    (is (= :bug (:type (detect/detect-report
                        {:email/subject     "[BUG] crash on save"
                         :email/attachments format-patch-attachment})))))

  (testing "Re: discussion + git-diff (no format-patch headers) attachment -> nil"
    (let [att-diff [{:attachment/filename "debug.patch"
                     :attachment/content-type "text/x-patch"
                     :attachment/data "diff --git a/foo b/foo\n--- a/foo\n+++ b/foo\n@@ -1 +1 @@\n-old\n+new\n"}]]
      (is (nil? (detect/detect-report
                 {:email/subject     "Re: Discussion"
                  :email/in-reply-to "<parent@test.org>"
                  :email/attachments att-diff}))))))

(deftest format-patch-headers-follow-matched-from-line-test
  (testing "headers are parsed after the matched From <sha> line, not
            from line 2: a preamble containing 'Subject:' must not
            pollute the result"
    (let [text (str "Quoted context follows.\n"
                    "Subject: wrong subject from the preamble\n"
                    "\n"
                    "From 0123456789abcdef0123456789abcdef01234567 Mon Sep 17 00:00:00 2026\n"
                    "From: Alice <alice@test.org>\n"
                    "Subject: [PATCH] Real subject\n\n"
                    "diff --git a/foo b/foo\n")]
      (is (= "[PATCH] Real subject"
             (:subject (detect/parse-format-patch-headers text))))
      (is (= "Alice <alice@test.org>"
             (:author (detect/parse-format-patch-headers text))))))
  (testing "nil when the From <sha> signature is absent"
    (is (nil? (detect/parse-format-patch-headers "Subject: [PATCH] x\n")))))

;; ---------------------------------------------------------------------------
;; Colon-based topic extraction (bone.detect)
;; ---------------------------------------------------------------------------

(deftest colon-topic-test
  (let [topic (fn [subject]
                (:topic (detect/detect-report {:email/subject subject})))]
    (testing "one colon token"
      (is (= "orgweb" (topic "[BUG] orgweb: broken layout"))))
    (testing "nested colon tokens all become topics"
      (is (= "topic1 topic2" (topic "[BUG] topic1: topic2: an explanation"))))
    (testing "a sentence colon is not a topic"
      (is (nil? (topic "[BUG] When I press RET: nothing happens"))))
    (testing "a URL colon is not a topic"
      (is (nil? (topic "[BUG] see https://example.org: broken"))))
    (testing "paths and dots are single topics"
      (is (= "net/mlx5e" (topic "[BUG] net/mlx5e: crash")))
      (is (= "org-demo.html" (topic "[BUG] org-demo.html: bad aside"))))
    (testing "a bracket later in the title does not hide the topic"
      (is (= "ox-html" (topic "[BUG] ox-html: export of [[link]] broken")))
      (is (= "org-agenda" (topic "[BUG] org-agenda: foo [bar] baz: qux")))
      (is (= "lisp/ox.el" (topic "[PATCH] lisp/ox.el: fix x[0]"))))))

;; ---------------------------------------------------------------------------
;; Inline patch extraction
;; ---------------------------------------------------------------------------

(deftest extract-inline-patch-test
  (let [diff ["diff --git a/lisp/x.el b/lisp/x.el"
              "--- a/lisp/x.el"
              "+++ b/lisp/x.el"
              "@@ -1,2 +1,2 @@"
              " (defun x ()"
              "-  1)"
              "+  2)"]
        body (fn [& parts] (str/join "\n" (apply concat parts)))]
    (testing "the patch starts at its first line"
      (is (= (str/join "\n" diff)
             (detect/extract-inline-patch (body ["Quick fix below." ""] diff)))))
    (testing "what follows the diff is not part of the patch"
      (is (= (str/join "\n" diff)
             (detect/extract-inline-patch
              (body ["Quick fix below." ""] diff
                    ["" "WDYT?"
                     "_______________________________________________"
                     "Some mailing list"
                     "https://lists.example.org/listinfo/some"])))))
    (testing "a blank context line inside the diff is kept"
      (is (= (str/join "\n" (concat diff ["" " (provide 'x)"]))
             (detect/extract-inline-patch
              (body diff ["" " (provide 'x)" "" "Thanks."])))))
    (testing "a rename without hunks is kept whole"
      (let [rename ["diff --git a/old.el b/new.el"
                    "similarity index 100%"
                    "rename from old.el"
                    "rename to new.el"]]
        (is (= (str/join "\n" rename)
               (detect/extract-inline-patch (body rename ["" "Thanks."]))))))
    (testing "no patch, no result"
      (is (nil? (detect/extract-inline-patch "Just prose.")))
      (is (nil? (detect/extract-inline-patch nil))))))
