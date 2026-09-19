(ns bone.common-test
  "Unit tests for bone.common pure functions:
  classify-delivery, classify-source, parse-duration-str,
  gate-related helpers, and build-source-map edge cases."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure.edn :as edn]
            [bone.common :as common]))

;; ---------------------------------------------------------------------------
;; parse-duration-str
;; ---------------------------------------------------------------------------

(deftest parse-duration-str-test
  (testing "single units"
    (is (= 1   (common/parse-duration-str "1d")))
    (is (= 7   (common/parse-duration-str "1w")))
    (is (= 30  (common/parse-duration-str "1m")))
    (is (= 365 (common/parse-duration-str "1y"))))

  (testing "zero value"
    (is (= 0 (common/parse-duration-str "0d"))))

  (testing "combined units"
    (is (= 37  (common/parse-duration-str "1m 1w")))
    (is (= 395 (common/parse-duration-str "1y 1m")))
    (is (= 402 (common/parse-duration-str "1y 1m 1w")))
    (is (= 403 (common/parse-duration-str "1y 1m 1w 1d"))))

  (testing "whitespace variations"
    (is (= 37 (common/parse-duration-str "1m1w")))
    (is (= 37 (common/parse-duration-str "1m  1w"))))

  (testing "nil on empty/no-match input"
    (is (nil? (common/parse-duration-str "")))
    (is (nil? (common/parse-duration-str "hello"))))

  (testing "throws on unknown units"
    (is (thrown-with-msg? Exception #"Unknown duration unit"
                          (common/parse-duration-str "5h")))
    (is (thrown-with-msg? Exception #"Unknown duration unit"
                          (common/parse-duration-str "2x 1d"))))

  (testing "throws on junk after a valid token"
    (is (thrown-with-msg? Exception #"Invalid duration syntax"
                          (common/parse-duration-str "1d garbage")))))

;; ---------------------------------------------------------------------------
;; parse-iso-date
;; ---------------------------------------------------------------------------

(deftest parse-iso-date-test
  (testing "valid calendar dates"
    (is (some? (common/parse-iso-date "2026-02-28"))))
  (testing "invalid calendar dates are rejected, not normalized"
    (is (nil? (common/parse-iso-date "2026-02-31")))
    (is (nil? (common/parse-iso-date "2026-13-01")))))

;; ---------------------------------------------------------------------------
;; strip-signature
;; ---------------------------------------------------------------------------

(deftest strip-signature-test
  (testing "strips LF signature"
    (is (= "Hello world" (common/strip-signature "Hello world\n-- \nSignature content\nMore sig"))))

  (testing "strips CRLF signature"
    (is (= "Hello world" (common/strip-signature "Hello world\r\n-- \r\nSignature content\r\nMore sig"))))

  (testing "strips signature at the end of text without trailing newline"
    (is (= "Hello world" (common/strip-signature "Hello world\n-- "))))

  (testing "does not strip if no signature marker"
    (is (= "Hello world" (common/strip-signature "Hello world")))
    (is (= "Hello -- world" (common/strip-signature "Hello -- world"))))

  (testing "handles nil input gracefully"
    (is (nil? (common/strip-signature nil)))))

;; ---------------------------------------------------------------------------
;; parse-delay
;; ---------------------------------------------------------------------------

(deftest parse-delay-test
  (testing "integer passthrough"
    (is (= 42 (common/parse-delay 42)))
    (is (= 0  (common/parse-delay 0))))

  (testing "string delegation"
    (is (= 30 (common/parse-delay "1m")))
    (is (= 0  (common/parse-delay "0d"))))

  (testing "nil on unsupported types"
    (is (nil? (common/parse-delay :foo)))
    (is (nil? (common/parse-delay nil)))))

;; ---------------------------------------------------------------------------
;; classify-delivery
;; ---------------------------------------------------------------------------

(defn- make-headers [& kvs]
  (pr-str (vec (partition 2 kvs))))

(deftest classify-delivery-test
  (testing "List-Id header => :list"
    (is (= :list (common/classify-delivery
                  (make-headers "List-Id" "<bugs.example.org>")))))

  (testing "X-BeenThere header => :list"
    (is (= :list (common/classify-delivery
                  (make-headers "X-BeenThere" "list@example.org")))))

  (testing "original recipient not in To/Cc => :alias"
    (is (= :alias (common/classify-delivery
                   (make-headers "X-Original-To" "alias@example.org"
                                "To" "someone@else.org")))))

  (testing "original recipient in To => :direct"
    (is (= :direct (common/classify-delivery
                    (make-headers "X-Original-To" "me@example.org"
                                 "To" "me@example.org")))))

  (testing "multi-valued (vector) headers classify without crashing"
    ;; Repeated headers come back from get-header as vectors.
    (is (= :direct (common/classify-delivery
                    (make-headers "X-Original-To" ["me@example.org"]
                                  "To" ["me@example.org" "other@example.org"]))))
    (is (= :alias (common/classify-delivery
                   (make-headers "X-Original-To" ["alias@example.org" "x@y.org"]
                                 "To" ["someone@else.org"])))))

  (testing "no special headers => :direct"
    (is (= :direct (common/classify-delivery
                    (make-headers "To" "inbox@example.org")))))

  (testing "nil headers => :direct"
    (is (= :direct (common/classify-delivery nil)))))

;; ---------------------------------------------------------------------------
;; classify-source
;; ---------------------------------------------------------------------------

(def test-sources
  [{:name "my-list"  :list "bugs.example.org"}
   {:name "my-alias" :alias "sec@example.com"}
   {:name "my-box"   :to "inbox@example.com"}])

(deftest classify-source-test
  (testing "mailing list match via List-Id"
    (is (= "my-list"
           (common/classify-source
            (make-headers "List-Id" "<bugs.example.org>")
            test-sources))))

  (testing "alias match via X-Original-To"
    (is (= "my-alias"
           (common/classify-source
            (make-headers "X-Original-To" "sec@example.com"
                          "To" "other@example.com")
            test-sources))))

  (testing "mailbox match via Delivered-To"
    (is (= "my-box"
           (common/classify-source
            (make-headers "Delivered-To" "inbox@example.com")
            test-sources))))

  (testing "no match => nil"
    (is (nil? (common/classify-source
               (make-headers "To" "unknown@example.com")
               test-sources)))))

;; ---------------------------------------------------------------------------
;; source-type
;; ---------------------------------------------------------------------------

(deftest source-type-test
  (is (= :mailing-list (common/source-type {:list "x"})))
  (is (= :alias        (common/source-type {:alias "x"})))
  (is (= :mailbox      (common/source-type {:to "x"})))
  (is (nil?            (common/source-type {:name "broken"}))))

;; ---------------------------------------------------------------------------
;; build-source-map -- nil source-type exclusion (fix 11)
;; ---------------------------------------------------------------------------

(deftest build-source-map-excludes-nil-type
  (let [config {:sources [{:name "good" :list "bugs.example.org"}
                           {:name "bad"}]}
        sm     (common/build-source-map config)]
    (is (contains? sm "good"))
    (is (not (contains? sm "bad")))))

(deftest build-source-map-propagates-runtime-options
  (let [config {:restricted-types #{:bug}
                :patch-triggers? false
                :sources [{:name "global" :list "global.example.org"}
                          {:name "source" :list "source.example.org"
                           :restricted-types #{}
                           :patch-triggers? true}]}
        sm     (common/build-source-map config)]
    (testing "global values apply when a source does not override them"
      (is (= #{:bug} (get-in sm ["global" :restricted-types])))
      (is (false? (get-in sm ["global" :patch-triggers?]))))
    (testing "source values override globals, including empty/falsey values"
      (is (= #{} (get-in sm ["source" :restricted-types])))
      (is (true? (get-in sm ["source" :patch-triggers?]))))))

(deftest build-source-map-keeps-project-links
  (testing "the project-link keys survive the source-map whitelist --
            they feed the JSON envelope and the exported pages"
    (let [sm (common/build-source-map
              {:sources [{:name "s" :list "l.example.org"
                          :website        "https://p.example.org"
                          :contribute-url "https://p.example.org/contribute"
                          :post-address   "bugs@example.org"}]})]
      (is (= "https://p.example.org" (get-in sm ["s" :website])))
      (is (= "https://p.example.org/contribute" (get-in sm ["s" :contribute-url])))
      (is (= "bugs@example.org" (get-in sm ["s" :post-address]))))))

;; ---------------------------------------------------------------------------
;; slugify
;; ---------------------------------------------------------------------------

(deftest slugify-test
  (is (= "hello-world"  (common/slugify "Hello World")))
  (is (= "cafe"         (common/slugify "Café")))
  (is (= "a-b-c"        (common/slugify "a--b--c")))
  (is (= "test"         (common/slugify "-test-"))))

;; ---------------------------------------------------------------------------
;; header utilities
;; ---------------------------------------------------------------------------

(deftest get-header-case-insensitive
  (let [hdrs (make-headers "List-Id" "<test.example.org>"
                           "X-Custom" "value")]
    (is (= "<test.example.org>" (common/get-header hdrs "list-id")))
    (is (= "value" (common/get-header hdrs "x-custom")))
    (is (nil? (common/get-header hdrs "absent")))))

(deftest extract-list-id-test
  (is (= "bugs.example.org"
         (common/extract-list-id "Some list <bugs.example.org>")))
  (is (= "raw-value" (common/extract-list-id "raw-value")))
  (is (nil? (common/extract-list-id nil))))

(deftest extract-in-reply-to-test
  (let [hdrs (make-headers "In-Reply-To" " <msg@test> ")]
    (is (= "<msg@test>" (common/extract-in-reply-to hdrs))))
  (let [hdrs (make-headers "In-Reply-To" "  ")]
    (is (nil? (common/extract-in-reply-to hdrs))))
  (testing "parenthetical comment after bracketed id is stripped"
    (let [hdrs (make-headers "In-Reply-To" "<msg@test> (in reply to Joe)")]
      (is (= "<msg@test>" (common/extract-in-reply-to hdrs)))))
  (testing "folded whitespace inside value is tolerated"
    (let [hdrs (make-headers "In-Reply-To" "\n\t<msg@test>")]
      (is (= "<msg@test>" (common/extract-in-reply-to hdrs))))))

(deftest extract-bracketed-id-test
  (testing "clean bracketed id returns unchanged"
    (is (= "<abc@x>" (common/extract-bracketed-id "<abc@x>"))))
  (testing "first bracketed token wins when value carries extra tokens"
    (is (= "<abc@x>" (common/extract-bracketed-id "<abc@x> (comment) <ignored@y>"))))
  (testing "padded input is trimmed to the bracketed token"
    (is (= "<abc@x>" (common/extract-bracketed-id "   <abc@x>   "))))
  (testing "vector values take the first element"
    (is (= "<abc@x>" (common/extract-bracketed-id ["<abc@x>" "<other@y>"]))))
  (testing "rejects values without a bracketed token (RFC 5322 §3.6.4)"
    (is (nil? (common/extract-bracketed-id "  abc@x  ")))
    (is (nil? (common/extract-bracketed-id "no brackets here"))))
  (testing "rejects bracketed tokens with internal whitespace"
    (is (nil? (common/extract-bracketed-id "<foo bar@x>"))))
  (testing "lowercases the domain part (RFC 5322 §3.6.4)"
    (is (= "<abc@example.com>" (common/extract-bracketed-id "<abc@Example.COM>")))
    (is (= "<Local-Part@example.com>"
           (common/extract-bracketed-id "<Local-Part@EXAMPLE.com>"))))
  (testing "blank and nil return nil"
    (is (nil? (common/extract-bracketed-id nil)))
    (is (nil? (common/extract-bracketed-id "")))
    (is (nil? (common/extract-bracketed-id "   ")))))

(deftest extract-bracketed-id-raw-test
  (testing "preserves original case of the id-right"
    (is (= "<abc@Example.COM>" (common/extract-bracketed-id-raw "<abc@Example.COM>")))
    (is (= "<Local-Part@EXAMPLE.com>"
           (common/extract-bracketed-id-raw "<Local-Part@EXAMPLE.com>"))))
  (testing "first bracketed token wins, padding trimmed, vector accepted"
    (is (= "<abc@X>" (common/extract-bracketed-id-raw "<abc@X> (c) <other@Y>")))
    (is (= "<abc@X>" (common/extract-bracketed-id-raw "   <abc@X>   ")))
    (is (= "<abc@X>" (common/extract-bracketed-id-raw ["<abc@X>" "<other@Y>"]))))
  (testing "rejects malformed input identically to extract-bracketed-id"
    (is (nil? (common/extract-bracketed-id-raw "no brackets")))
    (is (nil? (common/extract-bracketed-id-raw "<foo bar@X>")))
    (is (nil? (common/extract-bracketed-id-raw nil)))
    (is (nil? (common/extract-bracketed-id-raw "")))
    (is (nil? (common/extract-bracketed-id-raw "   ")))))

(deftest normalize-mid-test
  (testing "lowercases the domain only, preserves local-part case"
    (is (= "<Abc@example.com>" (common/normalize-mid "<Abc@EXAMPLE.com>"))))
  (testing "no-op when no @"
    (is (= "<no-at>" (common/normalize-mid "<no-at>"))))
  (testing "uses the last @ as local/domain separator"
    (is (= "<a@b@c.com>" (common/normalize-mid "<a@b@C.COM>"))))
  (testing "nil passes through"
    (is (nil? (common/normalize-mid nil)))))

;; ---------------------------------------------------------------------------
;; Role checks (pure)
;; ---------------------------------------------------------------------------

(deftest maintainer-tenure-test
  (let [tenures [{:email "alice@test.org"
                  :from  #inst "2025-06-01T00:00:00Z"
                  :to    nil
                  :order 0}]]
    (testing "no date check => any active tenure matches"
      (is (common/maintainer? tenures "alice@test.org")))
    (testing "email after :from => true"
      (is (common/maintainer? tenures "alice@test.org"
                              #inst "2025-07-01T00:00:00Z")))
    (testing "email before :from => false"
      (is (not (common/maintainer? tenures "alice@test.org"
                                   #inst "2025-01-01T00:00:00Z")))))

  (testing "closed tenure does not match after :to"
    (let [tenures [{:email "bob@test.org"
                    :from  #inst "2025-01-01T00:00:00Z"
                    :to    #inst "2025-06-01T00:00:00Z"
                    :order 0}]]
      (is (common/maintainer? tenures "bob@test.org"
                              #inst "2025-03-01T00:00:00Z"))
      (is (not (common/maintainer? tenures "bob@test.org"
                                   #inst "2025-07-01T00:00:00Z"))))))

(deftest lead-maintainer-test
  (testing "earliest :from wins (nil sorts first)"
    (let [tenures [{:email "later@t.org" :from #inst "2025-06-01" :order 1}
                   {:email "first@t.org" :from nil                :order 0}
                   {:email "mid@t.org"   :from #inst "2025-03-01" :order 2}]]
      (is (= "first@t.org" (common/lead-maintainer tenures)))))
  (testing "tie-break by :order when :from is equal (both nil)"
    (let [tenures [{:email "b@t.org" :from nil :order 1}
                   {:email "a@t.org" :from nil :order 0}]]
      (is (= "a@t.org" (common/lead-maintainer tenures)))))
  (testing "closed tenures are ignored"
    (let [tenures [{:email "first@t.org" :from nil :to #inst "2025-01-01" :order 0}
                   {:email "now@t.org"   :from #inst "2025-02-01"          :order 1}]]
      (is (= "now@t.org" (common/lead-maintainer tenures)))))
  (testing "no active tenures => nil"
    (is (nil? (common/lead-maintainer []))))
  (testing "lead-maintainer? is case-insensitive"
    (let [tenures [{:email "lead@t.org" :from nil :order 0}]]
      (is (common/lead-maintainer? tenures "LEAD@t.org"))
      (is (not (common/lead-maintainer? tenures "other@t.org"))))))

;; ---------------------------------------------------------------------------
;; resolve-commands-map / resolve-command-overrides
;; ---------------------------------------------------------------------------

(deftest resolve-commands-map-test
  (let [cfg {:commands {:acked {:words ["LGTM" "Approved"]
                                :scope :maintainer}}}
        m   (common/resolve-commands-map cfg)]
    (is (= ["LGTM" "Approved"] (:acked m)))
    (testing "other defaults preserved"
      (is (seq (:closed m)))
      (is (seq (:owned m))))))

(deftest resolve-command-overrides-test
  (let [cfg {:commands {:acked {:words ["LGTM"]
                                :scope :maintainer
                                :report-types #{:bug}}}}
        ov  (common/resolve-command-overrides cfg)]
    (is (= :maintainer (:scope (:acked ov))))
    (is (= #{:bug} (:report-types (:acked ov))))))

;; ---------------------------------------------------------------------------
;; effective-source-config / reproducible-config
;; ---------------------------------------------------------------------------

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
        eff    (common/effective-source-config config "demo")]
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
    (testing "unknown source -> nil"
      (is (nil? (common/effective-source-config config "missing"))))))

(deftest effective-source-config-flat-command-override-test
  ;; The daemon replaces a command's {:scope :report-types} override
  ;; unit wholesale when the source redefines it (see
  ;; resolve-command-overrides): the published effective config must
  ;; not deep-merge the global :report-types back in.
  (let [config {:commands {:closed {:scope :maintainer :report-types [:bug]}}
                :sources  [{:name "demo" :list "demo.example.org"
                            :commands {:closed {:scope :anyone}}}]}
        eff    (common/effective-source-config config "demo")]
    (is (= {:closed {:scope :anyone}} (:commands eff))
        "the local override unit fully replaces the global one")))

(deftest reproducible-config-test
  (let [config {:sources [{:name "demo" :list "demo.example.org"}]}
        data   (common/reproducible-config config "demo")
        s      (common/reproducible-config-str config "demo")]
    (testing ":mailboxes is a single Maildir placeholder"
      (is (= 1 (count (:mailboxes data))))
      (is (= :maildir (:type (first (:mailboxes data))))))
    (testing ":sources carries the effective source"
      (is (= "demo.example.org" (:list (first (:sources data))))))
    (testing "string form is commented and parses back as the same data"
      (is (str/starts-with? s ";;"))
      (is (= data (edn/read-string s))))))

;; ---------------------------------------------------------------------------
;; resolve-command-syntax
;; ---------------------------------------------------------------------------

(deftest resolve-command-syntax-accepts-keywords
  (is (= :loose  (common/resolve-command-syntax {})))
  (is (= :loose  (common/resolve-command-syntax {:command-syntax :loose})))
  (is (= :strict (common/resolve-command-syntax {:command-syntax :strict}))))

;; ---------------------------------------------------------------------------
;; sent-via-source-channel?
;; ---------------------------------------------------------------------------

(deftest sent-via-source-channel-test
  (testing "mailing-list source: only :list delivery is via channel"
    (let [cfg {:source-type :mailing-list}]
      (is (true?  (common/sent-via-source-channel? :list cfg)))
      (is (false? (common/sent-via-source-channel? :direct cfg)))
      (is (false? (common/sent-via-source-channel? :alias cfg)))))

  (testing "alias source: only :alias delivery is via channel"
    (let [cfg {:source-type :alias}]
      (is (true?  (common/sent-via-source-channel? :alias cfg)))
      (is (false? (common/sent-via-source-channel? :direct cfg)))
      (is (false? (common/sent-via-source-channel? :list cfg)))))

  (testing "mailbox source: all deliveries are via channel"
    (let [cfg {:source-type :mailbox}]
      (is (true? (common/sent-via-source-channel? :direct cfg)))
      (is (true? (common/sent-via-source-channel? :list cfg)))
      (is (true? (common/sent-via-source-channel? :alias cfg)))))

  (testing "unknown source-type: always false"
    (is (false? (common/sent-via-source-channel? :direct {})))
    (is (false? (common/sent-via-source-channel? :list {})))))

;; ---------------------------------------------------------------------------
;; ics-file?
;; ---------------------------------------------------------------------------

;; ---------------------------------------------------------------------------
;; parse-cli-args -- --topics-filter
;; ---------------------------------------------------------------------------

(deftest parse-cli-args-topics-filter
  (is (= "event,security"
         (:topics-filter (common/parse-cli-args ["--topics-filter" "event,security"]))))
  (is (nil? (:topics-filter (common/parse-cli-args ["--force"])))))

;; ---------------------------------------------------------------------------
;; ics-file?
;; ---------------------------------------------------------------------------

(deftest ics-file-test
  (is (true?  (common/ics-file? "event.ics")))
  (is (true?  (common/ics-file? "Meeting.ICS")))
  (is (true?  (common/ics-file? "path/to/file.ics")))
  (is (false? (common/ics-file? "readme.txt")))
  (is (false? (common/ics-file? "ics-like.doc")))
  (is (false? (common/ics-file? nil))))

;; ---------------------------------------------------------------------------
;; text-attachment?
;; ---------------------------------------------------------------------------

(deftest text-attachment-test
  (is (true?  (common/text-attachment? {:attachment/content-type "text/plain"
                                        :attachment/filename     "backtrace.txt"})))
  (is (true?  (common/text-attachment? {:attachment/content-type "Text/X-Log; charset=utf-8"
                                        :attachment/filename     "build.log"})))
  ;; ingest asks about the content type alone
  (is (true?  (common/text-attachment? {:attachment/content-type "text/plain"})))
  (is (false? (common/text-attachment? {:attachment/content-type "application/pdf"
                                        :attachment/filename     "doc.pdf"})))
  (is (false? (common/text-attachment? {:attachment/filename "notes.txt"})))
  (testing "a patch sent as text/plain is a patch, not also a text attachment"
    (is (false? (common/text-attachment? {:attachment/content-type "text/plain"
                                          :attachment/filename     "0001-fix.patch"})))
    (is (false? (common/text-attachment? {:attachment/content-type "text/plain; charset=us-ascii"
                                          :attachment/filename     "changes.DIFF"})))))

;; ---------------------------------------------------------------------------
;; fold-ics-line
;; ---------------------------------------------------------------------------

(defn- physical-lines [folded] (str/split folded #"\r\n"))

(defn- octets [^String s] (alength (.getBytes s "UTF-8")))

(defn- unfold [folded] (str/replace folded "\r\n " ""))

(deftest fold-ics-line-test
  (let [emoji (String. (Character/toChars 0x1F600))] ; non-BMP, 4 UTF-8 octets
    (testing "long ASCII line folds to <= 75 octets per physical line"
      (let [line   (apply str (repeat 200 "a"))
            folded (common/fold-ics-line line)]
        (is (every? #(<= (octets %) 75) (physical-lines folded)))
        (is (= line (unfold folded)))))

    (testing "a fold never splits a surrogate pair (74 ASCII + emoji)"
      (let [line   (str (apply str (repeat 74 "a")) emoji)
            folded (common/fold-ics-line line)]
        (is (> (count (physical-lines folded)) 1) "the line did fold")
        (is (every? #(<= (octets %) 75) (physical-lines folded)))
        (is (= line (unfold folded)) "round-trip keeps the emoji intact")))

    (testing "a run of 30 emoji folds on code-point boundaries"
      (let [line   (apply str (repeat 30 emoji))
            folded (common/fold-ics-line line)]
        (is (> (count (physical-lines folded)) 1) "the line did fold")
        (is (every? #(<= (octets %) 75) (physical-lines folded)))
        (is (= line (unfold folded)))))))

;; ---------------------------------------------------------------------------
;; valid-config-name?
;; ---------------------------------------------------------------------------

(deftest valid-config-name-test
  (testing "accepts alphanum start, spaces/dots/dashes in the middle"
    (is (common/valid-config-name? "demo"))
    (is (common/valid-config-name? "a"))
    (is (common/valid-config-name? "My Source 2"))
    (is (common/valid-config-name? "a.b_c-d")))
  (testing "rejects trailing space, leading space and bad starts"
    (is (not (common/valid-config-name? "demo ")))
    (is (not (common/valid-config-name? " demo")))
    (is (not (common/valid-config-name? "-demo")))
    (is (not (common/valid-config-name? "")))
    (is (not (common/valid-config-name? nil)))))

;; ---------------------------------------------------------------------------
;; resolve-author -- Mailman/DMARC munging detection
;; ---------------------------------------------------------------------------

(deftest resolve-author-test
  (testing "no Reply-To: author = From"
    (is (= {:address "alice@example.org" :name "Alice"}
           (common/resolve-author
            {:from-address "alice@example.org"
             :from-name    "Alice"
             :reply-to     []}))))

  (testing "Reply-To present but From-name has no 'via' marker: keep From"
    ;; Standard Reply-To use (e.g. 'reply to my work address').  We must
    ;; NOT swap -- the actual author is in From.
    (is (= {:address "alice@example.org" :name "Alice"}
           (common/resolve-author
            {:from-address "alice@example.org"
             :from-name    "Alice"
             :reply-to     [{:address "alice-work@example.com"
                             :name    "Alice (work)"}]}))))

  (testing "Mailman/DMARC munging: From-name has 'via' + Reply-To different => use Reply-To"
    ;; Mirrors the BUG-org-habit fixture: Daniel Mendler's post via
    ;; gnu.org's mailman has From rewritten to the list address, and
    ;; the original sender address lives in Reply-To.
    (is (= {:address "mail@daniel-mendler.de" :name "Daniel Mendler"}
           (common/resolve-author
            {:from-address "emacs-orgmode@gnu.org"
             :from-name    "Daniel Mendler via \"General discussions about Org-mode.\""
             :reply-to     [{:address "mail@daniel-mendler.de"
                             :name    "Daniel Mendler"}]}))))

  (testing "munged From with no Reply-To: degrade to From rather than nil"
    (is (= {:address "list@example.org"
            :name    "Bob via \"Some List\""}
           (common/resolve-author
            {:from-address "list@example.org"
             :from-name    "Bob via \"Some List\""
             :reply-to     []}))))

  (testing "Reply-To name absent: fall back to From-name"
    (is (= {:address "real@author.io"
            :name    "Carol via \"L\""}
           (common/resolve-author
            {:from-address "list@example.org"
             :from-name    "Carol via \"L\""
             :reply-to     [{:address "real@author.io"}]}))))

  (testing "Reply-To equals From: no swap"
    (is (= {:address "alice@example.org" :name "Alice via \"L\""}
           (common/resolve-author
            {:from-address "alice@example.org"
             :from-name    "Alice via \"L\""
             :reply-to     [{:address "alice@example.org"}]})))))

;; ---------------------------------------------------------------------------
;; extract-trailers
;; ---------------------------------------------------------------------------

(deftest extract-trailers-test
  (testing "collects person trailers with canonical capitalization"
    (is (= ["Reviewed-by: Jane Doe <jane@example.org>"]
           (common/extract-trailers
            "Looks good to me.\n\nReviewed-by: Jane Doe <jane@example.org>\n")))
    (is (= ["Acked-by: Jo <jo@x.org>" "Tested-by: Sam <sam@y.org>"]
           (common/extract-trailers
            "acked-by: Jo <jo@x.org>\nTESTED-BY: Sam <sam@y.org>\n"))))

  (testing "quoted and indented lines are ignored"
    (is (empty? (common/extract-trailers
                 "> Reviewed-by: Jane <jane@example.org>\n"))
        "quoted trailer from the patch under review")
    (is (empty? (common/extract-trailers
                 "  Signed-off-by: Jane <jane@example.org>\n"))
        "indented lines are diff/quote context, not trailers"))

  (testing "values without an address are skipped"
    (is (empty? (common/extract-trailers "Tested-by: nobody yet\n"))))

  (testing "unknown keys are skipped"
    (is (empty? (common/extract-trailers "Fixed-by: Jane <jane@example.org>\n"))))

  (testing "scanning stops at a pasted patch (inline fixup leak)"
    (is (= ["Reviewed-by: R <r@x>"]
           (common/extract-trailers
            (str "Nice, but squash this fixup:\n\n"
                 "Reviewed-by: R <r@x>\n\n"
                 "From 0123456789abcdef0123456789abcdef01234567"
                 " Mon Sep 17 00:00:00 2001\n"
                 "From: U <u@x>\n"
                 "Subject: [PATCH] fixup\n\n"
                 "Signed-off-by: U <u@x>\n---\n"
                 "diff --git a/f b/f\n")))
        "the pasted patch's own Signed-off-by is not collected")
    (is (empty? (common/extract-trailers
                 "diff --git a/f b/f\nSigned-off-by: U <u@x>\n"))
        "a bare pasted diff truncates from its first line"))

  (testing "duplicates collapse, order of first appearance is kept"
    (is (= ["Reviewed-by: A <a@x>" "Acked-by: B <b@x>"]
           (common/extract-trailers
            "Reviewed-by: A <a@x>\nAcked-by: B <b@x>\nReviewed-by: A <a@x>\n"))))

  (testing "signature block is not scanned (stripped upstream by email-body-text)"
    (is (= ["Reviewed-by: A <a@x>"]
           (common/extract-trailers
            (common/strip-signature
             "Reviewed-by: A <a@x>\n-- \nSigned-off-by: Sig <sig@x>\n")))))

  (testing "nil body is safe"
    (is (nil? (common/extract-trailers nil)))))
