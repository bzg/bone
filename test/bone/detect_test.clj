(ns bone.detect-test
  "Unit tests for ICS detection helpers (common + detect)."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [bone.common :as common]
            [bone.detect :as detect]))

;; ---------------------------------------------------------------------------
;; ICS attachment detection (bone.common)
;; ---------------------------------------------------------------------------

(deftest has-ics-attachment-test
  (testing "detects .ics attachment"
    (is (common/has-ics-attachment?
         [{:attachment/filename "meeting.ics"}])))

  (testing "case-insensitive"
    (is (common/has-ics-attachment?
         [{:attachment/filename "Event.ICS"}])))

  (testing "no .ics attachment"
    (is (not (common/has-ics-attachment?
              [{:attachment/filename "readme.txt"}]))))

  (testing "nil attachments"
    (is (not (common/has-ics-attachment? nil))))

  (testing "empty attachments"
    (is (not (common/has-ics-attachment? [])))))

;; ---------------------------------------------------------------------------
;; Inline ICS detection (bone.common)
;; ---------------------------------------------------------------------------

(deftest has-inline-ics-test
  (testing "detects inline VCALENDAR with VEVENT"
    (is (common/has-inline-ics?
         "Some text\nBEGIN:VCALENDAR\nVERSION:2.0\nBEGIN:VEVENT\nSUMMARY:Test\nEND:VEVENT\nEND:VCALENDAR")))

  (testing "nil body"
    (is (not (common/has-inline-ics? nil))))

  (testing "VCALENDAR without VEVENT"
    (is (not (common/has-inline-ics?
              "BEGIN:VCALENDAR\nVERSION:2.0\nEND:VCALENDAR"))))

  (testing "plain text without ICS"
    (is (not (common/has-inline-ics? "Hello world")))))

;; ---------------------------------------------------------------------------
;; ICS parsing and assembly (bone.common)
;; ---------------------------------------------------------------------------

(deftest normalize-ics-eol-test
  (testing "lone LF becomes CRLF"
    (is (= "a\r\nb\r\n" (common/normalize-ics-eol "a\nb\n"))))
  (testing "existing CRLF is preserved (not doubled)"
    (is (= "a\r\nb" (common/normalize-ics-eol "a\r\nb"))))
  (testing "lone CR becomes CRLF"
    (is (= "a\r\nb" (common/normalize-ics-eol "a\rb"))))
  (testing "nil-safe"
    (is (nil? (common/normalize-ics-eol nil)))))

(deftest extract-vevents-test
  (testing "extracts a single VEVENT, CRLF-normalized and terminated"
    (is (= ["BEGIN:VEVENT\r\nUID:a@x\r\nEND:VEVENT\r\n"]
           (common/extract-vevents "BEGIN:VEVENT\nUID:a@x\nEND:VEVENT"))))
  (testing "extracts multiple adjacent VEVENTs without merging them"
    (is (= 2 (count (common/extract-vevents
                     (str "BEGIN:VEVENT\nUID:a\nEND:VEVENT\n"
                          "BEGIN:VEVENT\nUID:b\nEND:VEVENT\n"))))))
  (testing "no VEVENT yields nil"
    (is (nil? (common/extract-vevents "BEGIN:VCALENDAR\nEND:VCALENDAR")))
    (is (nil? (common/extract-vevents nil)))))

(deftest ics-property-test
  (testing "reads a property value, trimmed"
    (is (= "a@x" (common/ics-property "BEGIN:VEVENT\r\nUID:a@x\r\nEND:VEVENT\r\n" "UID"))))
  (testing "ignores parameters before the colon"
    (is (= "20260101T100000"
           (common/ics-property "BEGIN:VEVENT\r\nDTSTART;TZID=Europe/Paris:20260101T100000\r\nEND:VEVENT\r\n"
                                "DTSTART"))))
  (testing "case-insensitive property name"
    (is (= "5" (common/ics-property "uid:x\r\nSEQUENCE:5\r\n" "sequence"))))
  (testing "absent property yields nil"
    (is (nil? (common/ics-property "BEGIN:VEVENT\r\nUID:a\r\nEND:VEVENT\r\n" "SUMMARY")))))

(deftest dedupe-vevents-test
  (let [v0 "BEGIN:VEVENT\r\nUID:a@x\r\nSEQUENCE:0\r\nSUMMARY:old\r\nEND:VEVENT\r\n"
        v1 "BEGIN:VEVENT\r\nUID:a@x\r\nSEQUENCE:1\r\nSUMMARY:new\r\nEND:VEVENT\r\n"
        vb "BEGIN:VEVENT\r\nUID:b@x\r\nEND:VEVENT\r\n"]
    (testing "same UID collapses to the highest SEQUENCE"
      (is (= [v1] (common/dedupe-vevents [v0 v1]))))
    (testing "result keeps first-seen UID order"
      (is (= [v1 vb] (common/dedupe-vevents [v0 vb v1]))))
    (testing "distinct UID-less blocks are all kept"
      (let [n1 "BEGIN:VEVENT\r\nSUMMARY:x\r\nEND:VEVENT\r\n"
            n2 "BEGIN:VEVENT\r\nSUMMARY:y\r\nEND:VEVENT\r\n"]
        (is (= [n1 n2] (common/dedupe-vevents [n1 n2])))))
    (testing "identical UID-less blocks collapse to a single first-seen copy"
      (let [n1 "BEGIN:VEVENT\r\nSUMMARY:x\r\nEND:VEVENT\r\n"
            n2 "BEGIN:VEVENT\r\nSUMMARY:y\r\nEND:VEVENT\r\n"]
        (is (= [n1] (common/dedupe-vevents [n1 n1])))
        (is (= [n1 n2] (common/dedupe-vevents [n1 n2 n1])))))

    ;; Recurring event: the master and a per-occurrence override share the
    ;; same UID but differ by RECURRENCE-ID -- both must survive.
    (let [master   "BEGIN:VEVENT\r\nUID:r@x\r\nDTSTART:20260601T100000Z\r\nRRULE:FREQ=WEEKLY\r\nEND:VEVENT\r\n"
          occ      "BEGIN:VEVENT\r\nUID:r@x\r\nRECURRENCE-ID:20260608T100000Z\r\nSEQUENCE:0\r\nDTSTART:20260608T150000Z\r\nEND:VEVENT\r\n"
          occ-v1   "BEGIN:VEVENT\r\nUID:r@x\r\nRECURRENCE-ID:20260608T100000Z\r\nSEQUENCE:1\r\nDTSTART:20260608T160000Z\r\nEND:VEVENT\r\n"]
      (testing "master and per-occurrence override (same UID, distinct RECURRENCE-ID) are both kept"
        (is (= [master occ] (common/dedupe-vevents [master occ]))))
      (testing "two updates of the same occurrence collapse to the highest SEQUENCE"
        (is (= [occ-v1] (common/dedupe-vevents [occ occ-v1]))))
      (testing "master is independent of its occurrence overrides"
        (is (= [master occ-v1] (common/dedupe-vevents [master occ occ-v1])))))))

(deftest dedupe-vtimezones-test
  (let [paris  "BEGIN:VTIMEZONE\r\nTZID:Europe/Paris\r\nEND:VTIMEZONE\r\n"
        paris2 "BEGIN:VTIMEZONE\r\nTZID:Europe/Paris\r\nX-NOTE:dup\r\nEND:VTIMEZONE\r\n"
        utc    "BEGIN:VTIMEZONE\r\nTZID:UTC\r\nEND:VTIMEZONE\r\n"
        anon   "BEGIN:VTIMEZONE\r\nEND:VTIMEZONE\r\n"]
    (testing "one VTIMEZONE per TZID, first-seen"
      (is (= [paris utc] (common/dedupe-vtimezones [paris paris2 utc]))))
    (testing "anonymous (TZID-less) VTIMEZONE is dropped"
      (is (= [paris] (common/dedupe-vtimezones [anon paris]))))))

(deftest escape-ics-text-test
  (testing "comma and semicolon are backslash-escaped"
    (is (= "v1.0\\, final" (common/escape-ics-text "v1.0, final")))
    (is (= "a\\;b" (common/escape-ics-text "a;b"))))
  (testing "backslash is escaped first (no double-escaping of added escapes)"
    (is (= "back\\\\slash" (common/escape-ics-text "back\\slash"))))
  (testing "newlines become literal \\n"
    (is (= "l1\\nl2" (common/escape-ics-text "l1\nl2")))
    (is (= "l1\\nl2" (common/escape-ics-text "l1\r\nl2"))))
  (testing "plain text is untouched, nil-safe"
    (is (= "plain subject" (common/escape-ics-text "plain subject")))
    (is (nil? (common/escape-ics-text nil)))))

(deftest fold-ics-line-test
  (testing "short line is returned unchanged"
    (is (= "X-WR-CALNAME:hi" (common/fold-ics-line "X-WR-CALNAME:hi"))))
  (testing "nil-safe"
    (is (nil? (common/fold-ics-line nil))))
  (testing "long line is folded; no content line exceeds 75 octets"
    (let [line   (str "X-WR-CALNAME:" (apply str (repeat 200 "a")))
          folded (common/fold-ics-line line)]
      (is (str/includes? folded "\r\n "))
      (is (every? #(<= (alength (.getBytes ^String % "UTF-8")) 75)
                  (str/split folded #"\r\n")))))
  (testing "unfolding (CRLF + leading space removed) restores the original"
    (let [line   (str "X-WR-CALNAME:" (apply str (repeat 200 "a")))
          folded (common/fold-ics-line line)]
      (is (= line (str/replace folded #"\r\n " "")))))
  (testing "a multi-octet character is never split across a fold"
    ;; 40 'e accent aigu' = 80 UTF-8 octets, forcing at least one fold.
    (let [line   (str "X-WR-CALNAME:" (apply str (repeat 40 "é")))
          folded (common/fold-ics-line line)]
      (is (str/includes? folded "\r\n "))
      ;; round-trips cleanly: no é was cut in half
      (is (= line (str/replace folded #"\r\n " ""))))))

(deftest build-vcalendar-test
  (let [vevent "BEGIN:VEVENT\r\nUID:a@x\r\nEND:VEVENT\r\n"
        vtz    "BEGIN:VTIMEZONE\r\nTZID:Europe/Paris\r\nEND:VTIMEZONE\r\n"]
    (testing "nil when there is no event"
      (is (nil? (common/build-vcalendar "src events" [] [vtz]))))
    (testing "wraps VTIMEZONE before VEVENT inside one VCALENDAR"
      (let [doc (common/build-vcalendar "src events" [vevent] [vtz])]
        (is (str/starts-with? doc "BEGIN:VCALENDAR\r\n"))
        (is (str/ends-with? doc "END:VCALENDAR\r\n"))
        (is (str/includes? doc "X-WR-CALNAME:src events\r\n"))
        (is (< (.indexOf doc "BEGIN:VTIMEZONE")
               (.indexOf doc "BEGIN:VEVENT")))))
    (testing "a long calendar name is folded so no line exceeds 75 octets"
      (let [doc (common/build-vcalendar (apply str (repeat 120 "x")) [vevent] [])]
        (is (every? #(<= (alength (.getBytes ^String % "UTF-8")) 75)
                    (str/split doc #"\r\n")))))))

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
      (is (= "org-demo.html" (topic "[BUG] org-demo.html: bad aside"))))))

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
