(ns bone.ics-test
  "Unit tests for bone.ics: ICS detection, parsing, deduplication,
  folding and VCALENDAR assembly."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [bone.ics :as ics]))

;; ---------------------------------------------------------------------------
;; ICS attachment detection (bone.ics)
;; ---------------------------------------------------------------------------

(deftest ics-file-test
  (is (true?  (ics/ics-file? "event.ics")))
  (is (true?  (ics/ics-file? "Meeting.ICS")))
  (is (true?  (ics/ics-file? "path/to/file.ics")))
  (is (false? (ics/ics-file? "readme.txt")))
  (is (false? (ics/ics-file? "ics-like.doc")))
  (is (false? (ics/ics-file? nil))))

(deftest has-ics-attachment-test
  (testing "detects .ics attachment"
    (is (ics/has-ics-attachment?
         [{:attachment/filename "meeting.ics"}])))

  (testing "case-insensitive"
    (is (ics/has-ics-attachment?
         [{:attachment/filename "Event.ICS"}])))

  (testing "no .ics attachment"
    (is (not (ics/has-ics-attachment?
              [{:attachment/filename "readme.txt"}]))))

  (testing "nil attachments"
    (is (not (ics/has-ics-attachment? nil))))

  (testing "empty attachments"
    (is (not (ics/has-ics-attachment? [])))))

;; ---------------------------------------------------------------------------
;; Inline ICS detection (bone.ics)
;; ---------------------------------------------------------------------------

(deftest has-inline-ics-test
  (testing "detects inline VCALENDAR with VEVENT"
    (is (ics/has-inline-ics?
         "Some text\nBEGIN:VCALENDAR\nVERSION:2.0\nBEGIN:VEVENT\nSUMMARY:Test\nEND:VEVENT\nEND:VCALENDAR")))

  (testing "nil body"
    (is (not (ics/has-inline-ics? nil))))

  (testing "VCALENDAR without VEVENT"
    (is (not (ics/has-inline-ics?
              "BEGIN:VCALENDAR\nVERSION:2.0\nEND:VCALENDAR"))))

  (testing "plain text without ICS"
    (is (not (ics/has-inline-ics? "Hello world")))))

;; ---------------------------------------------------------------------------
;; ICS parsing and assembly (bone.ics)
;; ---------------------------------------------------------------------------

(deftest normalize-ics-eol-test
  (testing "lone LF becomes CRLF"
    (is (= "a\r\nb\r\n" (ics/normalize-ics-eol "a\nb\n"))))
  (testing "existing CRLF is preserved (not doubled)"
    (is (= "a\r\nb" (ics/normalize-ics-eol "a\r\nb"))))
  (testing "lone CR becomes CRLF"
    (is (= "a\r\nb" (ics/normalize-ics-eol "a\rb"))))
  (testing "nil-safe"
    (is (nil? (ics/normalize-ics-eol nil)))))

(deftest extract-vevents-test
  (testing "extracts a single VEVENT, CRLF-normalized and terminated"
    (is (= ["BEGIN:VEVENT\r\nUID:a@x\r\nEND:VEVENT\r\n"]
           (ics/extract-vevents "BEGIN:VEVENT\nUID:a@x\nEND:VEVENT"))))
  (testing "extracts multiple adjacent VEVENTs without merging them"
    (is (= 2 (count (ics/extract-vevents
                     (str "BEGIN:VEVENT\nUID:a\nEND:VEVENT\n"
                          "BEGIN:VEVENT\nUID:b\nEND:VEVENT\n"))))))
  (testing "no VEVENT yields nil"
    (is (nil? (ics/extract-vevents "BEGIN:VCALENDAR\nEND:VCALENDAR")))
    (is (nil? (ics/extract-vevents nil)))))

(deftest ics-property-test
  (testing "reads a property value, trimmed"
    (is (= "a@x" (ics/ics-property "BEGIN:VEVENT\r\nUID:a@x\r\nEND:VEVENT\r\n" "UID"))))
  (testing "ignores parameters before the colon"
    (is (= "20260101T100000"
           (ics/ics-property "BEGIN:VEVENT\r\nDTSTART;TZID=Europe/Paris:20260101T100000\r\nEND:VEVENT\r\n"
                                "DTSTART"))))
  (testing "case-insensitive property name"
    (is (= "5" (ics/ics-property "uid:x\r\nSEQUENCE:5\r\n" "sequence"))))
  (testing "absent property yields nil"
    (is (nil? (ics/ics-property "BEGIN:VEVENT\r\nUID:a\r\nEND:VEVENT\r\n" "SUMMARY")))))

(deftest dedupe-vevents-test
  (let [v0 "BEGIN:VEVENT\r\nUID:a@x\r\nSEQUENCE:0\r\nSUMMARY:old\r\nEND:VEVENT\r\n"
        v1 "BEGIN:VEVENT\r\nUID:a@x\r\nSEQUENCE:1\r\nSUMMARY:new\r\nEND:VEVENT\r\n"
        vb "BEGIN:VEVENT\r\nUID:b@x\r\nEND:VEVENT\r\n"]
    (testing "same UID collapses to the highest SEQUENCE"
      (is (= [v1] (ics/dedupe-vevents [v0 v1]))))
    (testing "result keeps first-seen UID order"
      (is (= [v1 vb] (ics/dedupe-vevents [v0 vb v1]))))
    (testing "distinct UID-less blocks are all kept"
      (let [n1 "BEGIN:VEVENT\r\nSUMMARY:x\r\nEND:VEVENT\r\n"
            n2 "BEGIN:VEVENT\r\nSUMMARY:y\r\nEND:VEVENT\r\n"]
        (is (= [n1 n2] (ics/dedupe-vevents [n1 n2])))))
    (testing "identical UID-less blocks collapse to a single first-seen copy"
      (let [n1 "BEGIN:VEVENT\r\nSUMMARY:x\r\nEND:VEVENT\r\n"
            n2 "BEGIN:VEVENT\r\nSUMMARY:y\r\nEND:VEVENT\r\n"]
        (is (= [n1] (ics/dedupe-vevents [n1 n1])))
        (is (= [n1 n2] (ics/dedupe-vevents [n1 n2 n1])))))

    ;; Recurring event: the master and a per-occurrence override share the
    ;; same UID but differ by RECURRENCE-ID -- both must survive.
    (let [master   "BEGIN:VEVENT\r\nUID:r@x\r\nDTSTART:20260601T100000Z\r\nRRULE:FREQ=WEEKLY\r\nEND:VEVENT\r\n"
          occ      "BEGIN:VEVENT\r\nUID:r@x\r\nRECURRENCE-ID:20260608T100000Z\r\nSEQUENCE:0\r\nDTSTART:20260608T150000Z\r\nEND:VEVENT\r\n"
          occ-v1   "BEGIN:VEVENT\r\nUID:r@x\r\nRECURRENCE-ID:20260608T100000Z\r\nSEQUENCE:1\r\nDTSTART:20260608T160000Z\r\nEND:VEVENT\r\n"]
      (testing "master and per-occurrence override (same UID, distinct RECURRENCE-ID) are both kept"
        (is (= [master occ] (ics/dedupe-vevents [master occ]))))
      (testing "two updates of the same occurrence collapse to the highest SEQUENCE"
        (is (= [occ-v1] (ics/dedupe-vevents [occ occ-v1]))))
      (testing "master is independent of its occurrence overrides"
        (is (= [master occ-v1] (ics/dedupe-vevents [master occ occ-v1])))))))

(deftest dedupe-vtimezones-test
  (let [paris  "BEGIN:VTIMEZONE\r\nTZID:Europe/Paris\r\nEND:VTIMEZONE\r\n"
        paris2 "BEGIN:VTIMEZONE\r\nTZID:Europe/Paris\r\nX-NOTE:dup\r\nEND:VTIMEZONE\r\n"
        utc    "BEGIN:VTIMEZONE\r\nTZID:UTC\r\nEND:VTIMEZONE\r\n"
        anon   "BEGIN:VTIMEZONE\r\nEND:VTIMEZONE\r\n"]
    (testing "one VTIMEZONE per TZID, first-seen"
      (is (= [paris utc] (ics/dedupe-vtimezones [paris paris2 utc]))))
    (testing "anonymous (TZID-less) VTIMEZONE is dropped"
      (is (= [paris] (ics/dedupe-vtimezones [anon paris]))))))

(deftest escape-ics-text-test
  (testing "comma and semicolon are backslash-escaped"
    (is (= "v1.0\\, final" (ics/escape-ics-text "v1.0, final")))
    (is (= "a\\;b" (ics/escape-ics-text "a;b"))))
  (testing "backslash is escaped first (no double-escaping of added escapes)"
    (is (= "back\\\\slash" (ics/escape-ics-text "back\\slash"))))
  (testing "newlines become literal \\n"
    (is (= "l1\\nl2" (ics/escape-ics-text "l1\nl2")))
    (is (= "l1\\nl2" (ics/escape-ics-text "l1\r\nl2"))))
  (testing "plain text is untouched, nil-safe"
    (is (= "plain subject" (ics/escape-ics-text "plain subject")))
    (is (nil? (ics/escape-ics-text nil)))))

(defn- physical-lines [folded] (str/split folded #"\r\n"))

(defn- octets [^String s] (alength (.getBytes s "UTF-8")))

(defn- unfold [folded] (str/replace folded "\r\n " ""))

(deftest fold-ics-line-test
  (testing "short line is returned unchanged"
    (is (= "X-WR-CALNAME:hi" (ics/fold-ics-line "X-WR-CALNAME:hi"))))
  (testing "nil-safe"
    (is (nil? (ics/fold-ics-line nil))))
  (testing "long ASCII line folds to <= 75 octets per physical line and unfolds back"
    (let [line   (str "X-WR-CALNAME:" (apply str (repeat 200 "a")))
          folded (ics/fold-ics-line line)]
      (is (> (count (physical-lines folded)) 1) "the line did fold")
      (is (every? #(<= (octets %) 75) (physical-lines folded)))
      (is (= line (unfold folded)))))
  (testing "a multi-octet character is never split across a fold"
    ;; 40 'e accent aigu' = 80 UTF-8 octets, forcing at least one fold.
    (let [line   (str "X-WR-CALNAME:" (apply str (repeat 40 "é")))
          folded (ics/fold-ics-line line)]
      (is (> (count (physical-lines folded)) 1) "the line did fold")
      (is (= line (unfold folded)) "no é was cut in half")))
  (let [emoji (String. (Character/toChars 0x1F600))] ; non-BMP, 4 UTF-8 octets
    (testing "a fold never splits a surrogate pair (74 ASCII + emoji)"
      (let [line   (str (apply str (repeat 74 "a")) emoji)
            folded (ics/fold-ics-line line)]
        (is (> (count (physical-lines folded)) 1) "the line did fold")
        (is (every? #(<= (octets %) 75) (physical-lines folded)))
        (is (= line (unfold folded)) "round-trip keeps the emoji intact")))
    (testing "a run of 30 emoji folds on code-point boundaries"
      (let [line   (apply str (repeat 30 emoji))
            folded (ics/fold-ics-line line)]
        (is (> (count (physical-lines folded)) 1) "the line did fold")
        (is (every? #(<= (octets %) 75) (physical-lines folded)))
        (is (= line (unfold folded)))))))

(deftest build-vcalendar-test
  (let [vevent "BEGIN:VEVENT\r\nUID:a@x\r\nEND:VEVENT\r\n"
        vtz    "BEGIN:VTIMEZONE\r\nTZID:Europe/Paris\r\nEND:VTIMEZONE\r\n"]
    (testing "nil when there is no event"
      (is (nil? (ics/build-vcalendar "src events" [] [vtz]))))
    (testing "wraps VTIMEZONE before VEVENT inside one VCALENDAR"
      (let [doc (ics/build-vcalendar "src events" [vevent] [vtz])]
        (is (str/starts-with? doc "BEGIN:VCALENDAR\r\n"))
        (is (str/ends-with? doc "END:VCALENDAR\r\n"))
        (is (str/includes? doc "X-WR-CALNAME:src events\r\n"))
        (is (< (.indexOf doc "BEGIN:VTIMEZONE")
               (.indexOf doc "BEGIN:VEVENT")))))
    (testing "a long calendar name is folded so no line exceeds 75 octets"
      (let [doc (ics/build-vcalendar (apply str (repeat 120 "x")) [vevent] [])]
        (is (every? #(<= (alength (.getBytes ^String % "UTF-8")) 75)
                    (str/split doc #"\r\n")))))))
