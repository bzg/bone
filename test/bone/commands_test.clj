(ns bone.commands-test
  "Unit tests for the loose/strict :command-syntax modes.

  The matrix being verified: every Bone command (bareword, negative
  bareword, -by line, Date/Topic/Expiry/Deadline line, Superseded-by,
  role command) must be:

    - accepted with and without `!` prefix in :loose mode
    - accepted only with `!` prefix in :strict mode"
  (:require [clojure.test :refer [deftest is testing]]
            [bone.commands :as commands]
            [bone.roles :as roles]))

(defn- src-cmds [strict?]
  (commands/build-source-commands
   {:command-syntax (if strict? :strict :loose)}))

;; ---------------------------------------------------------------------------
;; Barewords (simple + negative)
;; ---------------------------------------------------------------------------

(deftest word-loose-accepts-both-forms
  (let [sc (src-cmds false)]
    (testing "bare form"
      (is (some? (commands/detect-words :bug "Closed.\n" sc))))
    (testing "!-prefixed form"
      (is (some? (commands/detect-words :bug "!Closed.\n" sc))))))

(deftest word-strict-rejects-bare-form
  (let [sc (src-cmds true)]
    (testing "bare form rejected"
      (is (nil? (commands/detect-words :bug "Closed.\n" sc))))
    (testing "!-prefixed form accepted"
      (is (some? (commands/detect-words :bug "!Closed.\n" sc))))))

;; ---------------------------------------------------------------------------
;; Colon-line commands (-by, Deadline, Topic, Superseded-by) and negatives
;; ---------------------------------------------------------------------------

(defn- detect-with [strict? body]
  (let [sc (src-cmds strict?)]
    (commands/detect-lines :bug body nil nil (:line-patterns sc))))

(deftest line-loose-accepts-both-forms
  (testing "-by line"
    (is (seq (detect-with false "Acked-by: a@b.com\n")))
    (is (seq (detect-with false "!Acked-by: a@b.com\n"))))
  (testing "Deadline"
    (is (seq (detect-with false "Deadline: 2026-06-15\n")))
    (is (seq (detect-with false "!Deadline: 2026-06-15\n"))))
  (testing "Negative unset (Not X)"
    (is (seq (detect-with false "Not acked\n")))
    (is (seq (detect-with false "!Not acked\n"))))
  (testing "No X form"
    (is (seq (detect-with false "No deadline\n")))
    (is (seq (detect-with false "!No deadline\n"))))
  (testing "Superseded-by"
    (is (seq (detect-with false "Superseded-by: <msg@example.com>\n")))
    (is (seq (detect-with false "!Superseded-by: <msg@example.com>\n"))))
  (testing "Supersedes"
    (is (seq (detect-with false "Supersedes: <msg@example.com>\n")))
    (is (seq (detect-with false "!Supersedes: <msg@example.com>\n"))))
  (testing "Not supersedes"
    (is (seq (detect-with false "Not supersedes: <msg@example.com>\n")))
    (is (seq (detect-with false "!Not supersedes: <msg@example.com>\n")))))

(deftest line-strict-rejects-bare-form
  (testing "-by line"
    (is (empty? (detect-with true "Acked-by: a@b.com\n")))
    (is (seq    (detect-with true "!Acked-by: a@b.com\n"))))
  (testing "Deadline"
    (is (empty? (detect-with true "Deadline: 2026-06-15\n")))
    (is (seq    (detect-with true "!Deadline: 2026-06-15\n"))))
  (testing "Not acked"
    (is (empty? (detect-with true "Not acked\n")))
    (is (seq    (detect-with true "!Not acked\n"))))
  (testing "Superseded-by"
    (is (empty? (detect-with true "Superseded-by: <msg@example.com>\n")))
    (is (seq    (detect-with true "!Superseded-by: <msg@example.com>\n"))))
  (testing "Supersedes"
    (is (empty? (detect-with true "Supersedes: <msg@example.com>\n")))
    (is (seq    (detect-with true "!Supersedes: <msg@example.com>\n"))))
  (testing "Not supersedes"
    (is (empty? (detect-with true "Not supersedes: <msg@example.com>\n")))
    (is (seq    (detect-with true "!Not supersedes: <msg@example.com>\n")))))

;; ---------------------------------------------------------------------------
;; Role directives (Add/Remove maintainer) via pure parse-role-controls
;; ---------------------------------------------------------------------------

(deftest role-loose-accepts-both-forms
  (is (= 1 (count (roles/parse-role-controls "Add maintainer: a@b.com" false))))
  (is (= 1 (count (roles/parse-role-controls "!Add maintainer: a@b.com" false)))))

(deftest role-strict-rejects-bare-form
  (is (= 0 (count (roles/parse-role-controls "Add maintainer: a@b.com" true))))
  (is (= 1 (count (roles/parse-role-controls "!Add maintainer: a@b.com" true)))))

;; ---------------------------------------------------------------------------
;; Role control pattern -- strict mode requires the bang prefix
;; ---------------------------------------------------------------------------

(deftest role-control-pattern-mode-aware
  (let [loose  (roles/role-control-pattern false)
        strict (roles/role-control-pattern true)]
    (is (some? (re-find loose "Add maintainer: x@y.z")))
    (is (some? (re-find loose "!Add maintainer: x@y.z")))
    (is (nil?  (re-find strict "Add maintainer: x@y.z")))
    (is (some? (re-find strict "!Add maintainer: x@y.z")))))

;; ---------------------------------------------------------------------------
;; build-source-commands shape
;; ---------------------------------------------------------------------------

(deftest build-source-commands-shape
  (testing "scalar :strict produces strict-syntax? true"
    (let [sc (commands/build-source-commands {:command-syntax :strict})]
      (is (true? (:strict-syntax? sc)))
      (is (some? (:word-patterns sc)))
      (is (some? (:line-patterns sc)))))
  (testing "scalar :loose produces strict-syntax? false"
    (let [sc (commands/build-source-commands {:command-syntax :loose})]
      (is (false? (:strict-syntax? sc)))))
  (testing "no :command-syntax defaults to loose"
    (let [sc (commands/build-source-commands {})]
      (is (false? (:strict-syntax? sc))))))

;; ---------------------------------------------------------------------------
;; Case-insensitive address caches
;; ---------------------------------------------------------------------------

(deftest word-tx-lowercases-address-cache
  (testing "build-word-tx stores :report/*-address lowercased regardless of
            the sender's from-address casing -- so downstream :setter-or-maintainer
            comparisons are stable even if the user's MUA rewrites address case"
    (let [word-result {:report/acked 999}
          [tx _ _] (commands/build-word-tx 42 word-result 999 "Alice@Example.COM" {})]
      (is (some (fn [datom] (= datom [:db/add 42 :report/acked-address "alice@example.com"]))
                tx)))))

;; ---------------------------------------------------------------------------
;; Reviewed-by (kernel-style synonym of Acked-by) and Reviewed bareword
;; ---------------------------------------------------------------------------

(deftest reviewed-by-sets-acked
  (testing "Reviewed-by: is a syntax synonym of Acked-by:"
    (is (= [{:action :set :attr :report/acked :email-address "x@y.com"
             :scope :user :id :acked-by}]
           (vec (detect-with false "Reviewed-by: x@y.com\n"))))
    (testing "kernel form with a display name"
      (is (= [{:action :set :attr :report/acked :email-address "x@y.com"
               :scope :user :id :acked-by}]
             (vec (detect-with false "Reviewed-by: Jane Doe <x@y.com>\n")))))
    (testing "Acked-by: keeps working"
      (is (= [{:action :set :attr :report/acked :email-address "x@y.com"
               :scope :user :id :acked-by}]
             (vec (detect-with false "Acked-by: x@y.com\n")))))
    (testing "strict mode still requires the ! prefix"
      (is (empty? (detect-with true "Reviewed-by: x@y.com\n")))
      (is (seq (detect-with true "!Reviewed-by: x@y.com\n"))))))

(deftest reviewed-bareword-is-not-a-default-acked-word
  ;; In loose mode a reply opener like "Reviewed v2 and found
  ;; problems." would ack with inverse polarity, so only the full
  ;; Reviewed-by: line is recognized by default; sources can add the
  ;; bareword via :words.
  (let [sc (src-cmds false)]
    (is (nil? (commands/detect-words :patch "Reviewed.\n" sc)))
    (is (nil? (commands/detect-words :patch "Reviewed v2 and found problems.\n" sc)))
    (testing "a source can opt in via :words"
      (let [sc (commands/build-source-commands
                {:command-syntax :loose
                 :commands {:acked {:words ["Acked" "Reviewed"]}}})]
        (is (some? (commands/detect-words :patch "Reviewed.\n" sc)))))))

(deftest not-word-negations-follow-the-vocabulary
  (testing "every default acked word has its Not form"
    (doseq [line ["Not acked.\n" "Not confirmed.\n" "Not approved.\n"]]
      (is (= [{:action :unset :attr :report/acked
               :scope :user :id :unacked}]
             (vec (detect-with false line)))
          line)))
  (testing "per-source synonyms gain their negation"
    (let [sc (commands/build-source-commands
              {:command-syntax :loose
               :commands {:owned {:words ["Owned" "Handled"]}}})]
      (is (= [{:action :unset :attr :report/owned
               :scope :user :id :unowned}]
             (vec (commands/detect-lines :bug "Not handled.\n" nil nil
                                         (:line-patterns sc)))))))
  (testing "strict mode still requires the ! prefix"
    (is (empty? (detect-with true "Not confirmed.\n")))
    (is (seq (detect-with true "!Not confirmed.\n"))))
  (testing "prose lines with trailing text do not match"
    (is (empty? (detect-with false "Not confirmed yet, will retest tomorrow.\n")))))

;; ---------------------------------------------------------------------------
;; Pure scope partitions and vote tx (decision layer of the ! functions)
;; ---------------------------------------------------------------------------

(deftest partition-words-by-scope-pure
  (let [wr {:report/closed true :report/acked true
            :report/close-reason :resolved}]
    (testing "default :user scope allows everything"
      (is (= {:allowed wr :denied []}
             (commands/partition-words-by-scope wr {} false))))
    (testing "a :maintainer override denies non-maintainers only"
      (let [{:keys [allowed denied]}
            (commands/partition-words-by-scope
             wr {:closed {:scope :maintainer}} false)]
        (is (= [:report/closed] denied))
        ;; close-reason is no bareword: it rides back onto the allowed
        ;; map and is only consumed when :report/closed is set.
        (is (= {:report/acked true :report/close-reason :resolved} allowed))))
    (testing "maintainers pass the same override"
      (is (= {:allowed wr :denied []}
             (commands/partition-words-by-scope
              wr {:closed {:scope :maintainer}} true))))
    (testing "everything denied yields a nil allowed map"
      (is (nil? (:allowed (commands/partition-words-by-scope
                           {:report/closed true}
                           {:closed {:scope :maintainer}} false)))))))

(deftest partition-lines-by-scope-pure
  (let [lines [{:id :closed-by :action :set :attr :report/closed
                :scope :maintainer :email-address "a@b.c"}
               {:id :acked-by :action :set :attr :report/acked
                :scope :user :email-address "a@b.c"}]
        {:keys [allowed denied]}
        (commands/partition-lines-by-scope lines (delay {}) "x@y.z" false
                                           (constantly true))]
    (is (= [:acked-by] (mapv :id allowed)))
    (is (= [:closed-by] (mapv :id denied)))
    (testing "line-pred applies before the scope split"
      (is (= {:allowed [] :denied []}
             (commands/partition-lines-by-scope lines (delay {}) "x@y.z" false
                                                (constantly false)))))))

(deftest detect-vote-ignores-code-and-patches
  (testing "numbers in code or markup are not votes"
    (is (nil? (commands/detect-vote "  (S-left lambda nil (org-ref-swap-citation-link -1))")))
    (is (nil? (commands/detect-vote "1-\\nu & \\nu & 0 \\\\")))
    (is (nil? (commands/detect-vote "see +1/-1 in the table"))))
  (testing "hunk headers of a pasted patch are not votes"
    (is (nil? (commands/detect-vote
               "Quick fix below.\n\ndiff --git a/x.el b/x.el\n--- a/x.el\n+++ b/x.el\n@@ -1 +1 @@\n-a\n+b")))
    (is (nil? (commands/detect-vote "@@ -1,7 +1,7 @@ (defun foo ()"))))
  (testing "a vote before the pasted patch still counts"
    (is (= :up (commands/detect-vote
                "+1, and here is a fixup:\n\ndiff --git a/x.el b/x.el\n@@ -1 +1 @@\n-a\n+b"))))
  (testing "votes followed by punctuation still count"
    (is (= :up (commands/detect-vote "+1!!")))
    (is (= :down (commands/detect-vote "-1: this breaks the agenda")))))

(deftest detect-vote-ignores-quoted-lines
  (testing "a vote appearing only in quoted text does not count"
    (is (nil? (commands/detect-vote "> +1")))
    (is (nil? (commands/detect-vote "Bob wrote:\n> +1\n\nAgreed, but no vote here."))))
  (testing "a quoted vote cannot invert the sender's actual vote"
    (is (= :down (commands/detect-vote "> +1\n-1 from me")))
    (is (= :down (commands/detect-vote "Bob wrote:\n> +1\n\n-1 from me."))))
  (testing "unquoted votes still detect"
    (is (= :up (commands/detect-vote "+1")))
    (is (= :up (commands/detect-vote "I agree\n+1\nthanks")))))

(deftest vote-tx-pure
  (testing "first vote creates the voter's entity"
    (is (= {:vote/key "k" :vote/report 1 :vote/email 2
            :vote/value :up :vote/voter "a@b.c"}
           (commands/vote-tx {:vote-key "k" :report-eid 1 :email-eid 2
                              :voter-addr "a@b.c" :existing nil
                              :current nil :vote :up}))))
  (testing "a revote targets the existing entity by :db/id"
    (is (= {:db/id 7 :vote/email 2 :vote/value :down}
           (commands/vote-tx {:vote-key "k" :report-eid 1 :email-eid 2
                              :voter-addr "a@b.c" :existing 7
                              :current :up :vote :down}))))
  (testing "an unchanged revote is a no-op"
    (is (nil? (commands/vote-tx {:existing 7 :current :up :vote :up})))))
