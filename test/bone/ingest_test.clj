(ns bone.ingest-test
  "Unit tests for bone.ingest -- ICS attachment data extraction
  and composite-source dedup."
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [datalevin.core :as d]
            [bone.common :as common]
            [bone.ingest :as ingest]))

;; ---------------------------------------------------------------------------
;; ICS attachment data storage
;; ---------------------------------------------------------------------------

(deftest ics-attachment-data-stored
  (testing ".ics attachment text data is stored"
    (let [ics-data "BEGIN:VCALENDAR\r\nVERSION:2.0\r\nBEGIN:VEVENT\r\nSUMMARY:Meeting\r\nEND:VEVENT\r\nEND:VCALENDAR\r\n"
          msg {:uid 100
               :message-id "<ics-test@test.org>"
               :subject "[ANN] Team meeting"
               :from [{:address "alice@test.org" :name "Alice"}]
               :date-sent #inst "2026-01-03T10:00:00Z"
               :body {:text "Please find the invite attached."
                      :attachments [{:filename "meeting.ics"
                                     :content-type "text/calendar"
                                     :data ics-data}]}}
          tx (ingest/email->txdata msg)]
      (is (= ics-data
             (-> tx :email/attachments first :attachment/data)))))

  (testing ".ics attachment bytes data is converted to string"
    (let [ics-data "BEGIN:VCALENDAR\r\nEND:VCALENDAR\r\n"
          msg {:uid 101
               :message-id "<ics-bytes@test.org>"
               :subject "[ANN] Event"
               :from [{:address "bob@test.org"}]
               :body {:text "Event invite."
                      :attachments [{:filename "event.ics"
                                     :content-type "text/calendar"
                                     :data (.getBytes ics-data "UTF-8")}]}}
          tx (ingest/email->txdata msg)]
      (is (= ics-data
             (-> tx :email/attachments first :attachment/data)))))

  (testing "non-ics non-patch attachment data is NOT stored"
    (let [msg {:uid 102
               :message-id "<pdf@test.org>"
               :subject "A document"
               :from [{:address "carol@test.org"}]
               :body {:text "See attached."
                      :attachments [{:filename "document.pdf"
                                     :content-type "application/pdf"
                                     :data "binary-data"}]}}
          tx (ingest/email->txdata msg)]
      (is (nil? (-> tx :email/attachments first :attachment/data)))))

  (testing ".patch attachment data is still stored"
    (let [patch-data "diff --git a/file b/file\n--- a/file\n+++ b/file\n"
          msg {:uid 103
               :message-id "<patch@test.org>"
               :subject "[PATCH] fix something"
               :from [{:address "dev@test.org"}]
               :body {:text "Patch attached."
                      :attachments [{:filename "fix.patch"
                                     :content-type "text/x-diff"
                                     :data patch-data}]}}
          tx (ingest/email->txdata msg)]
      (is (= patch-data
             (-> tx :email/attachments first :attachment/data)))))

  (testing "oversized attachment data is not stored"
    (let [big-data (apply str (repeat (inc ingest/default-max-attachment-size) "X"))
          msg {:uid 104
               :message-id "<big-ics@test.org>"
               :subject "[ANN] Huge event"
               :from [{:address "alice@test.org"}]
               :body {:text "Big invite."
                      :attachments [{:filename "huge.ics"
                                     :content-type "text/calendar"
                                     :data big-data}]}}
          tx (ingest/email->txdata msg)]
      (is (nil? (-> tx :email/attachments first :attachment/data)))
      (is (= "huge.ics" (-> tx :email/attachments first :attachment/filename)))))

  (testing "text/plain attachment data is stored"
    (let [txt-data "Exception in thread \"main\" java.lang.NullPointerException\n\tat Foo.bar(Foo.java:42)\n"
          msg {:uid 105
               :message-id "<txt@test.org>"
               :subject "[BUG] crash on startup"
               :from [{:address "user@test.org"}]
               :body {:text "See backtrace attached."
                      :attachments [{:filename "backtrace.txt"
                                     :content-type "text/plain"
                                     :data txt-data}]}}
          tx (ingest/email->txdata msg)]
      (is (= txt-data
             (-> tx :email/attachments first :attachment/data)))))

  (testing "text/x-log attachment data is stored"
    (let [log-data "2026-03-30 10:00:00 ERROR something broke\n"
          msg {:uid 106
               :message-id "<log@test.org>"
               :subject "[BUG] error in logs"
               :from [{:address "user@test.org"}]
               :body {:text "Log file attached."
                      :attachments [{:filename "error.log"
                                     :content-type "text/x-log"
                                     :data log-data}]}}
          tx (ingest/email->txdata msg)]
      (is (= log-data
             (-> tx :email/attachments first :attachment/data)))))

  (testing "text/plain with charset parameter is stored"
    (let [txt-data "some text content"
          msg {:uid 107
               :message-id "<txt-charset@test.org>"
               :subject "[BUG] encoding issue"
               :from [{:address "user@test.org"}]
               :body {:text "Attached."
                      :attachments [{:filename "output.txt"
                                     :content-type "text/plain; charset=utf-8"
                                     :data txt-data}]}}
          tx (ingest/email->txdata msg)]
      (is (= txt-data
             (-> tx :email/attachments first :attachment/data))))))

;; ---------------------------------------------------------------------------
;; Author resolution at ingest (Reply-To override on Mailman/DMARC munging)
;; ---------------------------------------------------------------------------

(deftest author-fields-on-ingest
  (testing "no munging: author fields mirror From"
    (let [msg {:message-id "<plain@test.org>"
               :subject    "Hello"
               :from       [{:address "alice@example.org" :name "Alice"}]}
          tx (ingest/email->txdata msg)]
      (is (= "alice@example.org" (:email/from-address tx)))
      (is (= "alice@example.org" (:email/author-address tx)))
      (is (= "Alice"             (:email/from-name tx)))
      (is (= "Alice"             (:email/author-name tx)))
      (is (nil? (:email/reply-to-address tx)))))

  (testing "Reply-To set without 'via' marker: author stays From, reply-to stored raw"
    (let [msg {:message-id "<rto@test.org>"
               :subject    "Hi"
               :from       [{:address "alice@example.org" :name "Alice"}]
               :reply-to   [{:address "alice-work@example.com"
                             :name    "Alice (work)"}]}
          tx (ingest/email->txdata msg)]
      (is (= "alice@example.org"      (:email/author-address tx)))
      (is (= "alice-work@example.com" (:email/reply-to-address tx)))
      (is (= "Alice (work)"           (:email/reply-to-name tx)))))

  (testing "Mailman/DMARC munging: author resolves to Reply-To"
    ;; Mirrors the BUG-org-habit fixture in the repo root.
    (let [msg {:message-id "<87ms7n6t47.fsf@daniel-mendler.de>"
               :subject    "[BUG] org-habit"
               :from       [{:address "emacs-orgmode@gnu.org"
                             :name    "Daniel Mendler via \"General discussions about Org-mode.\""}]
               :reply-to   [{:address "mail@daniel-mendler.de"
                             :name    "Daniel Mendler"}]}
          tx (ingest/email->txdata msg)]
      (is (= "emacs-orgmode@gnu.org"   (:email/from-address tx)))
      (is (= "mail@daniel-mendler.de"  (:email/author-address tx)))
      (is (= "Daniel Mendler"          (:email/author-name tx)))
      (is (= "mail@daniel-mendler.de"  (:email/reply-to-address tx))))))

;; ---------------------------------------------------------------------------
;; Composite (source, id) dedup -- two sources may legitimately collide on
;; mailseq id (IMAP UIDs are per-folder, Maildir filenames are per-folder),
;; so the dedup check must be source-scoped.
;; ---------------------------------------------------------------------------

(defn- fresh-conn []
  (let [path (str "/tmp/bone-ingest-test-" (System/currentTimeMillis) "-" (rand-int 1e6))]
    {:conn (d/get-conn path common/bone-schema) :path path}))

(defn- cleanup! [{:keys [conn path]}]
  (d/close conn)
  (let [dir (io/file path)]
    (when (.exists dir)
      (doseq [f (reverse (file-seq dir))] (.delete f)))))

(deftest email->txdata-references-header-case
  (doseq [k ["References" "references" "REFERENCES"]]
    (testing (str "header written as " k)
      (let [tx (ingest/email->txdata
                {:uid 200
                 :message-id "<child@test.org>"
                 :subject "Re: thread"
                 :from [{:address "alice@test.org"}]
                 :headers {k "<root@test.org> <parent@test.org>"}
                 :body {:text "Reply."}})]
        (is (= "<root@test.org> <parent@test.org>" (:email/references tx)))
        (is (= 2 (count (:email/ancestor-mid-hashes tx)))))))
  (testing "a repeated header is joined"
    (is (= "<root@test.org> <parent@test.org>"
           (:email/references
            (ingest/email->txdata
             {:uid 201
              :message-id "<child2@test.org>"
              :subject "Re: thread"
              :from [{:address "alice@test.org"}]
              :headers {"References" ["<root@test.org>" "<parent@test.org>"]}
              :body {:text "Reply."}}))))))

(defn- minimal-msg
  "Minimal mailseq-shaped msg with stable defaults."
  [{:keys [id mid]}]
  {:id           id
   :message-id   mid
   :subject      "Hi"
   :content-type "text/plain"
   :from         [{:address "alice@example.org" :name "Alice"}]
   :date-sent    #inst "2026-05-01"
   :body         {:text "Body."}})

(deftest store-email!-composite-source-id-dedup
  (testing "same id on different sources is NOT a collision"
    (let [{:keys [conn] :as setup} (fresh-conn)]
      (try
        (is (true? (ingest/store-email! conn
                                        (minimal-msg {:id "42" :mid "<m1@x>"})
                                        {:source "src-a"})))
        (is (true? (ingest/store-email! conn
                                        (minimal-msg {:id "42" :mid "<m2@x>"})
                                        {:source "src-b"})))
        (let [db (d/db conn)
              cnt (count (d/q '[:find [?e ...] :where [?e :email/message-id]] db))]
          (is (= 2 cnt) "both emails stored"))
        (finally (cleanup! setup)))))

  (testing "same id on the same source IS a collision (different Message-ID)"
    (let [{:keys [conn] :as setup} (fresh-conn)]
      (try
        (is (true? (ingest/store-email! conn
                                        (minimal-msg {:id "42" :mid "<m1@x>"})
                                        {:source "src-a"})))
        (is (false? (ingest/store-email! conn
                                         (minimal-msg {:id "42" :mid "<m3@x>"})
                                         {:source "src-a"})))
        (finally (cleanup! setup)))))

  (testing "same Message-ID on different sources is dropped as a dup (mid is globally unique)"
    (let [{:keys [conn] :as setup} (fresh-conn)]
      (try
        (is (true? (ingest/store-email! conn
                                        (minimal-msg {:id "42" :mid "<shared@x>"})
                                        {:source "src-a"})))
        (let [r (ingest/store-email! conn
                                     (minimal-msg {:id "99" :mid "<shared@x>"})
                                     {:source "src-b"})]
          ;; store-email! returns false on dup Message-ID -- that's the
          ;; intended branch ("re-process the existing entity"), not an
          ;; error.
          (is (false? r)))
        (finally (cleanup! setup))))))

;; ---------------------------------------------------------------------------
;; Undigested email ids -- Maildir catch-up re-presents these so a
;; failed digest (:retry) is not lost behind the seen/known baseline.
;; ---------------------------------------------------------------------------

(deftest undigested-email-ids-tracks-digested-at
  (testing "stored emails without :email/digested-at are reported"
    (let [{:keys [conn] :as setup} (fresh-conn)]
      (try
        (is (= #{} (ingest/undigested-email-ids conn)) "empty DB => empty set")
        (is (true? (ingest/store-email! conn
                                        (minimal-msg {:id "u1" :mid "<u1@x>"})
                                        {:source "src-a"})))
        (is (true? (ingest/store-email! conn
                                        (minimal-msg {:id "u2" :mid "<u2@x>"})
                                        {:source "src-a"})))
        (is (= #{"u1" "u2"} (ingest/undigested-email-ids conn)))
        ;; Stamping one as digested drops it from the set.
        (let [eid (d/q '[:find ?e . :in $ ?id :where [?e :email/id ?id]]
                       (d/db conn) "u1")]
          (d/transact! conn [{:db/id eid
                              :email/digested-at (java.util.Date.)}]))
        (is (= #{"u2"} (ingest/undigested-email-ids conn)))
        (finally (cleanup! setup))))))

;; ---------------------------------------------------------------------------
;; Per-mailbox watermarks -- multi-mailbox isolation
;; ---------------------------------------------------------------------------

(deftest watermarks-are-scoped-per-mailbox
  (testing "IMAP UID watermark is independent across mailboxes"
    (let [{:keys [conn] :as setup} (fresh-conn)]
      (try
        (is (= 0 (ingest/max-imap-uid conn "alpha")))
        (is (= 0 (ingest/max-imap-uid conn "beta")))
        (ingest/save-imap-uid! conn "alpha" 42)
        (ingest/save-imap-uid! conn "beta" 7)
        (is (= 42 (ingest/max-imap-uid conn "alpha")))
        (is (= 7  (ingest/max-imap-uid conn "beta")))
        ;; Updating one doesn't disturb the other.
        (ingest/save-imap-uid! conn "alpha" 100)
        (is (= 100 (ingest/max-imap-uid conn "alpha")))
        (is (= 7   (ingest/max-imap-uid conn "beta")))
        (finally (cleanup! setup)))))

  (testing "UIDVALIDITY is tracked per mailbox; reset clears only its own UID"
    (let [{:keys [conn] :as setup} (fresh-conn)]
      (try
        (ingest/save-imap-uid! conn "alpha" 50)
        (ingest/save-imap-uid! conn "beta" 80)
        (is (= :stamped (ingest/sync-uid-validity! conn "alpha" 1000)))
        (is (= :stamped (ingest/sync-uid-validity! conn "beta" 2000)))
        (is (= :match   (ingest/sync-uid-validity! conn "alpha" 1000)))
        ;; Bumping alpha's UIDVALIDITY resets alpha's UID, leaves beta alone.
        (is (= :reset   (ingest/sync-uid-validity! conn "alpha" 1001)))
        (is (= 0  (ingest/max-imap-uid conn "alpha")))
        (is (= 80 (ingest/max-imap-uid conn "beta")))
        (finally (cleanup! setup)))))

  (testing "Maildir init flag and seen-ids baseline are scoped per mailbox"
    (let [{:keys [conn] :as setup} (fresh-conn)]
      (try
        (is (false? (ingest/maildir-init-done? conn "alpha")))
        (is (false? (ingest/maildir-init-done? conn "beta")))
        (ingest/mark-ids-seen! conn "alpha" ["m1" "m2"])
        (ingest/set-maildir-init-done! conn "alpha")
        (is (= #{"m1" "m2"} (ingest/seen-maildir-ids conn "alpha")))
        (is (= #{}          (ingest/seen-maildir-ids conn "beta")))
        (is (true?  (ingest/maildir-init-done? conn "alpha")))
        (is (false? (ingest/maildir-init-done? conn "beta")))
        ;; Seeding beta separately leaves alpha's baseline intact.
        (ingest/mark-ids-seen! conn "beta" ["x1"])
        (is (= #{"m1" "m2"} (ingest/seen-maildir-ids conn "alpha")))
        (is (= #{"x1"}      (ingest/seen-maildir-ids conn "beta")))
        (finally (cleanup! setup)))))

  (testing "mark-ids-seen! accumulates across successive calls (cardinality/many)"
    (let [{:keys [conn] :as setup} (fresh-conn)]
      (try
        (ingest/mark-ids-seen! conn "alpha" ["m1" "m2"])
        (ingest/mark-ids-seen! conn "alpha" ["m3"])
        (is (= #{"m1" "m2" "m3"} (ingest/seen-maildir-ids conn "alpha")))
        ;; Re-asserting an existing id is a no-op (still the same set).
        (ingest/mark-ids-seen! conn "alpha" ["m2"])
        (is (= #{"m1" "m2" "m3"} (ingest/seen-maildir-ids conn "alpha")))
        (finally (cleanup! setup))))))
