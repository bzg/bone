(ns bone.digest-test
  "Integration tests for bone.digest.
  Creates a temporary Datalevin DB, inserts test emails, processes them
  via bone.digest/process-email!, and verifies reports, commands,
  threading, votes, roles, and permissions.

  Ported from test/bone-digest-test.clj (bb version)."
  (:require [clojure.edn :as edn]
            [clojure.string :as str]
            [clojure.test :refer [are deftest is testing use-fixtures]]
            [datalevin.core :as d]
            [bone.commands :as commands]
            [bone.common :as common]
            [bone.lookup :as lookup]
            [bone.digest :as digest]
            [bone.roles :as roles]
            [bone.test-helpers :as th])
  (:import [java.text SimpleDateFormat]
           [java.util TimeZone]))

(use-fixtures :once th/with-temp-failures-file)

;; ---------------------------------------------------------------------------
;; Helpers
;; ---------------------------------------------------------------------------

(defn- parse-date-iso [s]
  (let [fmt (doto (SimpleDateFormat. "yyyy-MM-dd")
              (.setTimeZone (TimeZone/getTimeZone "UTC")))]
    (.parse fmt s)))

(defn- get-report
  "Pull a report by message-id, including the qualified-relation reverse refs.
  After the qualified-links refactor: :report/superseded-by(-target) and
  :report/related are gone -- see :rel/_from / :rel/_to instead."
  [db message-id]
  (when-let [eid (lookup/report-eid db message-id)]
    (d/pull db
          '[:report/type :report/version :report/topic-value
            :report/patch-seq :report/patch-source :report/message-id
            {:report/patches [:patch/filename :patch/source :patch/author
                              :patch/subject :patch/date :patch/text]}
            :report/acked :report/owned :report/closed
            :report/close-reason
            :report/urgent :report/important
            :report/deadline-value :report/expiry-value
            :report/trailers
            :report/acked-address :report/owned-address
            :report/closed-address :report/urgent-address
            :report/important-address
            {:report/descendants [:email/message-id]}
            {:rel/_from [:rel/kind :rel/active? :rel/setter :rel/posed-at :rel/value
                         {:rel/to [:db/id :report/type :report/message-id
                                   {:report/email [:email/subject]}]}]}
            {:rel/_to [:rel/kind :rel/active? :rel/setter :rel/posed-at :rel/value
                       {:rel/from [:db/id :report/type :report/message-id
                                   {:report/email [:email/subject]}]}]}
            {:report/series [:series/id :series/expected :series/closed
                             {:series/patches [:db/id]}
                             {:series/cover-letter [:email/message-id]}]}
            {:report/email [:email/subject :email/author-address
                            :email/headers-edn]}]
          eid)))

;; --- Test helpers to aggregate :rel/* facts for assertion convenience ---

(defn- active-out-rels
  "All active outgoing relations of `kind` from `report-pull`."
  [report-pull kind]
  (->> (:rel/_from report-pull)
       (filter :rel/active?)
       (filter #(= kind (:rel/kind %)))))

(defn- all-related
  "Aggregate-related accessor: collects every report linked via
  :related-to (canonical may live on either side), :resolves/:resolved-by,
  or :supersedes/:superseded-by -- mirrors what a bidirectional
  :report/related ref would surface."
  [report-pull]
  (let [outs (->> (:rel/_from report-pull)
                  (filter :rel/active?)
                  (map :rel/to))
        ins  (->> (:rel/_to report-pull)
                  (filter :rel/active?)
                  (map :rel/from))]
    (vec (distinct (concat outs ins)))))

(defn- superseded-by-target [report-pull]
  (some-> (first (active-out-rels report-pull :supersedes)) :rel/to))

(defn- report-exists? [db message-id]
  (some? (lookup/report-eid db message-id)))

(defn- get-votes
  "Return all votes for a report as a seq of {:value :voter} maps."
  [db message-id]
  (let [rid (lookup/report-eid db message-id)]
    (when rid
      (mapv (fn [[val voter]]
              {:value val :voter voter})
            (d/q '[:find ?val ?voter :in $ ?r
                   :where [?v :vote/report ?r] [?v :vote/value ?val] [?v :vote/voter ?voter]]
                 db rid)))))

(defn- get-series-by-id [db sid]
  (when-let [eid (d/q '[:find ?s . :in $ ?sid :where [?s :series/id ?sid]] db sid)]
    (d/pull db
            '[:series/id :series/topic :series/sender :series/expected
              :series/closed :series/patches
              {:series/cover-letter [:email/message-id]}]
            eid)))

(defn- series-patch-count [db sid]
  (when-let [eid (d/q '[:find ?s . :in $ ?sid :where [?s :series/id ?sid]] db sid)]
    (count (d/q '[:find [?r ...] :in $ ?s :where [?s :series/patches ?r]]
                db eid))))

(defn- get-participant
  "Return the participant entity for a given source and email address, or nil."
  [db source-name email]
  (let [k (str source-name ":" (.toLowerCase email))]
    (when-let [eid (d/q '[:find ?e . :in $ ?k :where [?e :participant/key ?k]] db k)]
      (d/pull db '[:participant/key :participant/source :participant/email
                   :participant/name :participant/since :participant/contributor-since]
              eid))))

;; ---------------------------------------------------------------------------
;; Fixture data
;; ---------------------------------------------------------------------------

(def source-map {"direct"      {:admin "admin@test.org"
                                :source-type :mailbox
                                :to "direct@test.org"}
                 "public-list" {:admin "admin@test.org"
                                :source-type :mailing-list
                                :list "list.test.org"}
                 "no-triggers" {:admin "admin@test.org"
                                :source-type :mailbox
                                :to "no-triggers@test.org"
                                :patch-triggers? false}})

(def sources [{:name "public-list"
               :list "list.test.org"}
              {:name "direct"
               :to "direct@test.org"}
              {:name "no-triggers"
               :to "no-triggers@test.org"
               :patch-triggers? false}])

;; ---------------------------------------------------------------------------
;; DB setup / teardown
;; ---------------------------------------------------------------------------

(defn- setup-db! []
  (let [db-path (str "/tmp/bone-test-" (System/currentTimeMillis))
        conn    (d/get-conn db-path common/bone-schema)]
    ;; Seed an initial open tenure (from = nil, order = 0) making
    ;; admin@test.org the lead maintainer on both sources.
    (d/transact! conn [{:maint-tenure/source "direct"
                        :maint-tenure/email  "admin@test.org"
                        :maint-tenure/order  0}
                       {:maint-tenure/source "public-list"
                        :maint-tenure/email  "admin@test.org"
                        :maint-tenure/order  0}
                       {:maint-tenure/source "no-triggers"
                        :maint-tenure/email  "admin@test.org"
                        :maint-tenure/order  0}])
    ;; Insert test emails. Fixtures only set :email/from-address /
    ;; :email/from-name; mirror them into :email/author-* (the cleaned
    ;; identity attrs that downstream code reads) so the fixtures stay
    ;; concise and a fixture explicitly setting :email/author-* (e.g.
    ;; for a DMARC-munged scenario) overrides this default.
    (let [emails (edn/read-string (slurp "resources/emails.edn"))]
      (doseq [email emails]
        (d/transact!
         conn
         [(cond-> email
            ;; Derive the lookup hash like ingest/email->txdata does;
            ;; fixtures only carry the raw mid.
            (:email/message-id email)
            (assoc :email/message-id-hash
                   (common/mid-hash (:email/message-id email)))
            (and (:email/from-address email) (not (:email/author-address email)))
            (assoc :email/author-address (:email/from-address email))
            (and (:email/from-name email) (not (:email/author-name email)))
            (assoc :email/author-name (:email/from-name email)))])))
    {:conn conn :db-path db-path}))

(def ^:private teardown! th/teardown!)

(defn- process-all-emails!
  "Fetch all emails sorted by ingested-at, process each via digest/process-email!."
  [conn]
  (let [db     (d/db conn)
        emails (d/q '[:find [(pull ?e [:db/id :email/ingested-at :email/date-sent]) ...]
                      :where [?e :email/message-id _]]
                    db)
        sorted (sort-by #(or (:email/ingested-at %) (:email/date-sent %) (java.util.Date. 0))
                        emails)]
    (doseq [{eid :db/id} sorted]
      (let [email (d/pull (d/db conn) digest/email-pull-pattern eid)]
        (try
          (digest/process-email! conn source-map sources email)
          (catch Exception e
            (println "Error processing" (:email/message-id email) (.getMessage e))))))))

;; ---------------------------------------------------------------------------
;; Tests
;; ---------------------------------------------------------------------------

(deftest detect-vote-test
  (testing "Up votes"
    (is (= :up (commands/detect-vote "+1")))
    (is (= :up (commands/detect-vote "+1\n")))
    (is (= :up (commands/detect-vote "+1 thanks")))
    (is (= :up (commands/detect-vote "+1, great idea")))
    (is (= :up (commands/detect-vote "+1.")))
    (is (= :up (commands/detect-vote "+1!")))
    (is (= :up (commands/detect-vote "1+")))
    (is (= :up (commands/detect-vote "1+ ")))
    (is (= :up (commands/detect-vote "I agree\n+1\nthanks"))))

  (testing "Down votes"
    (is (= :down (commands/detect-vote "-1")))
    (is (= :down (commands/detect-vote "1-")))
    (is (= :down (commands/detect-vote "-1 nope")))
    (is (= :down (commands/detect-vote "1-."))))

  (testing "Null votes"
    (is (= :null (commands/detect-vote "+0")))
    (is (= :null (commands/detect-vote "-0")))
    (is (= :null (commands/detect-vote "0+")))
    (is (= :null (commands/detect-vote "0-")))
    (is (= :null (commands/detect-vote "+0, meh"))))

  (testing "Non-votes"
    (is (nil? (commands/detect-vote "+10")))
    (is (nil? (commands/detect-vote "+1abc")))
    (is (nil? (commands/detect-vote "1+2")))
    (is (nil? (commands/detect-vote "-10")))
    (is (nil? (commands/detect-vote "-1a")))
    (is (nil? (commands/detect-vote "1-2")))
    (is (nil? (commands/detect-vote "+0x")))
    (is (nil? (commands/detect-vote "nothing here")))
    (is (nil? (commands/detect-vote "")))
    (is (nil? (commands/detect-vote nil)))))

(deftest word-scope-denial-recording
  (testing "non-maintainer bareword on a source that overrides :closed to :maintainer"
    (let [filter-words      (var-get #'bone.commands/filter-words-by-scope)
          describe-word     (var-get #'bone.commands/describe-denied-word)
          recorded          (atom [])]
      (testing "describe-denied-word labels"
        (is (= "Closed." (describe-word :report/closed)))
        (is (= "Acked." (describe-word :report/acked)))
        (is (= "Urgent." (describe-word :report/urgent))))
      (with-redefs [commands/record-failure! (fn [entry] (swap! recorded conj entry))]
        (let [word-result {:report/closed true :report/acked true}
              overrides   {:closed {:scope :maintainer}}
              failure-ctx {:source "src" :from-addr "user@test" :email-date nil :report-mid "<r@test>"}
              filtered    (filter-words word-result overrides false failure-ctx)]
          (is (= {:report/acked true} filtered)
              "acked (default :user scope) survives, closed (maintainer-only override) is dropped")
          (is (= 1 (count @recorded)) "exactly one failure recorded")
          (let [entry (first @recorded)]
            (is (= :insufficient-scope (:reason entry)))
            (is (= :maintainers (:audience entry)))
            (is (= "Closed." (:command entry)))
            (is (= "user@test" (:from-addr entry)))
            (is (= "src" (:source entry)))))))))

(deftest digest-integration-test
  (let [{:keys [conn] :as ctx} (setup-db!)]
    (try
      (process-all-emails! conn)
      (let [db (d/db conn)]

        ;; --- Roles ---
        (testing "Roles (final state)"
          (let [ts (roles/get-tenures db "direct")]
            (is (= "admin@test.org" (common/lead-maintainer ts)))
            (is (common/maintainer? ts "maint@test.org"))))

        ;; --- Bug 02 lifecycle ---
        (testing "Bug 02 lifecycle"
          (let [r (get-report db "<02@test.org>")]
            (is (= :bug (:report/type r)))
            (is (= "9.7" (:report/topic-value r)))
            (is (some? (:report/acked r)))
            (is (some? (:report/owned r)))
            (is (some? (:report/closed r)))
            (is (= :resolved (:report/close-reason r)))
            (is (some? (:report/urgent r)))
            (is (= 3 (count (:report/descendants r))))))

        ;; --- Bug 03 mailing list prefix ---
        (testing "Bug 03 mailing list prefix"
          (let [r (get-report db "<03@test.org>")]
            (is (= :bug (:report/type r)))
            (is (nil? (:report/topic-value r)))))

        ;; --- Patch 07 subject detection ---
        (testing "Patch 07 subject detection"
          (let [r (get-report db "<07@test.org>")]
            (is (= :patch (:report/type r)))
            (is (= "org-agenda" (:report/topic-value r)))
            (is (= "1/2" (:report/patch-seq r)))
            (is (contains? (set (:report/patch-source r)) :subject))
            (is (some? (:report/acked r)))
            (is (some? (:report/closed r)))
            (is (= :resolved (:report/close-reason r)))))

        ;; --- Patch 08 attachment ---
        (testing "Patch 08 attachment detection"
          (let [r (get-report db "<08@test.org>")]
            (is (= :patch (:report/type r)))
            (is (contains? (set (:report/patch-source r)) :attachment))))

        ;; --- Email 09 inline diff, no label: no report (inline-only is not a signal) ---
        (testing "Inline diff alone (no label, no attachment) does not create a patch report"
          (is (false? (report-exists? db "<09@test.org>"))))

        ;; --- POLL 11 votes ---
        (testing "POLL 11 votes"
          (let [r     (get-report db "<11@test.org>")
                votes (get-votes db "<11@test.org>")]
            (is (= :request (:report/type r)))
            (is (= 2 (count votes)))
            (is (= 1 (count (filter #(= :up (:value %)) votes))))
            (is (= 1 (count (filter #(= :down (:value %)) votes))))
            (is (= #{"voter1@test.org" "voter2@test.org"}
                   (set (map :voter votes))))
            (is (= 3 (count (:report/descendants r))))))

        ;; --- Email 15 request lifecycle ---
        (testing "Email 15 request lifecycle"
          (let [r (get-report db "<15@test.org>")]
            (is (= :request (:report/type r)))
            (is (some? (:report/closed r)))
            (is (= :resolved (:report/close-reason r)))))

        ;; --- ANN 17 canceled ---
        (testing "ANN 17 canceled"
          (let [r (get-report db "<17@test.org>")]
            (is (= :announcement (:report/type r)))
            (is (some? (:report/closed r)))
            (is (= :canceled (:report/close-reason r)))))

        ;; --- ANN 18 denied ---
        (testing "ANN 18 permission denied"
          (is (not (report-exists? db "<18@test.org>"))))

        ;; --- CHG 19 / REL 20 ---
        (testing "CHG 19 / REL 20 release closes change"
          (let [chg (get-report db "<19@test.org>")
                rel (get-report db "<20@test.org>")]
            (is (= :change (:report/type chg)))
            (is (= "9.8" (:report/version chg)))
            (is (some? (:report/closed chg)))
            (is (= :resolved (:report/close-reason chg)))
            (is (= :release (:report/type rel)))
            (is (= "9.8" (:report/version rel)))
            (is (some #(= "<19@test.org>" (:report/message-id %))
                      (all-related rel)))
            (is (some #(= "<20@test.org>" (:report/message-id %))
                      (all-related chg)))))

        ;; --- Email 21 (Ignore mechanism removed -- report is now created) ---
        (testing "Email 21 spam user creates report (no Ignore)"
          (is (report-exists? db "<21@test.org>")))

        ;; --- Bug 23 important ---
        (testing "Bug 23 important flag"
          (let [r (get-report db "<23@test.org>")]
            (is (= :bug (:report/type r)))
            (is (some? (:report/important r)))
            (is (= 3 (count (:report/descendants r))))))

        ;; --- FR 26 ---
        (testing "FR 26 request"
          (is (= :request (:report/type (get-report db "<26@test.org>")))))

        ;; --- ANN 27 mailing list ---
        (testing "ANN 27 mailing list + Archived-At"
          (let [r (get-report db "<27@test.org>")]
            (is (= :announcement (:report/type r)))
            (let [headers (some-> (get-in r [:report/email :email/headers-edn])
                                  edn/read-string)]
              (is (= "https://list.example.org/archive/27"
                     (get headers "Archived-At"))))))

        ;; --- Patch 28 mailing list prefix ---
        (testing "Patch 28 mailing list prefix"
          (let [r (get-report db "<28@test.org>")]
            (is (= :patch (:report/type r)))
            (is (= "refactor" (:report/topic-value r)))
            (is (= "2/3" (:report/patch-seq r)))))

        ;; --- Email 29 role control via mailing list (now allowed) ---
        (testing "Email 29 role control via mailing list"
          (is (common/maintainer? (roles/get-tenures db "direct") "evil@hacker.org")))

        ;; --- Email 30 bug via list ---
        (testing "Email 30 bug via list"
          (is (= :bug (:report/type (get-report db "<30@test.org>")))))

        ;; --- Emails 112-113: channel gating ---
        (testing "Channel gating on public-list source"
          (let [r (get-report db "<30@test.org>")]
            ;; Only the public reply (112) is a descendant;
            ;; the private reply (113) is excluded entirely.
            (is (= 1 (count (:report/descendants r))))
            (is (= "<112@test.org>"
                    (:email/message-id (first (:report/descendants r)))))
            ;; Only public commands apply: 112 => acked (Confirmed.)
            ;; Private commands (113 => Owned.) are now blocked.
            (is (some? (:report/acked r)))
            (is (nil? (:report/owned r)))))

        ;; --- Email 31 bypassing list ---
        (testing "Email 31 bypassing list"
          (is (not (report-exists? db "<31@test.org>"))))

        ;; --- Email 32 wrong list ---
        (testing "Email 32 wrong List-Post"
          (is (not (report-exists? db "<32@test.org>"))))

        ;; --- Email 33 admin direct (no bone-source fallback) ---
        (testing "Email 33 admin direct -- no report without list delivery"
          (is (not (report-exists? db "<33@test.org>"))))

        ;; --- Series v1 ---
        (testing "Series v1 emails 34-37"
          (let [v1 (get-series-by-id db "parser|user@test.org|3")]
            (is (= "parser" (:series/topic v1)))
            (is (= 3 (:series/expected v1)))
            (is (= "<34@test.org>" (get-in v1 [:series/cover-letter :email/message-id])))
            (is (= 3 (series-patch-count db "parser|user@test.org|3")))
            (is (some? (:series/closed v1))))
          (let [r35 (get-report db "<35@test.org>")
                r37 (get-report db "<37@test.org>")]
            (is (some? (:report/series r35)))
            (is (= "1/3" (:report/patch-seq r35)))
            ;; All 3 patches in the series are mutually related
            (is (some #(= "<36@test.org>" (:report/message-id %))
                      (all-related r35)))
            (is (some #(= "<37@test.org>" (:report/message-id %))
                      (all-related r35)))
            (is (some #(= "<35@test.org>" (:report/message-id %))
                      (all-related r37)))))

        ;; --- Series v2 ---
        (testing "Series v2 emails 38-39"
          (let [v2 (get-series-by-id db "parser|user@test.org|3#2")]
            (is (some? (:series/id v2)))
            (is (nil? (:series/closed v2)))
            (is (= 3 (:series/expected v2)))
            (is (= "<38@test.org>" (get-in v2 [:series/cover-letter :email/message-id])))
            (is (= 1 (series-patch-count db "parser|user@test.org|3#2"))))
          (let [r39 (get-report db "<39@test.org>")]
            (is (some? (:report/series r39)))
            (is (= "1/3" (:report/patch-seq r39)))))

        ;; --- Series with diverging per-patch :topic (emails 125-127) ---
        (testing "Series held together by threading despite divergent topics"
          ;; Cover letter and patches each have a different `subdir:`
          ;; colon-prefix, so the per-message :topic differs across
          ;; reports. Threading via In-Reply-To must keep all patches
          ;; in the cover letter's series.
          (let [s (get-series-by-id db "orgweb|kana@test.org|2")]
            (is (some? (:series/id s)))
            (is (= 2 (:series/expected s)))
            (is (= "<125@test.org>" (get-in s [:series/cover-letter :email/message-id])))
            (is (= 2 (series-patch-count db "orgweb|kana@test.org|2"))))
          (let [r126 (get-report db "<126@test.org>")
                r127 (get-report db "<127@test.org>")]
            (is (some? (:report/series r126)))
            (is (some? (:report/series r127)))
            (is (= "orgweb|kana@test.org|2"
                   (:series/id (:report/series r126))))
            (is (= "orgweb|kana@test.org|2"
                   (:series/id (:report/series r127))))))

        ;; --- :patch-triggers? false on source (emails 128-129) ---
        (testing "Source with :patch-triggers? false skips auto-credit"
          (let [bug   (get-report db "<128@test.org>")
                patch (get-report db "<129@test.org>")]
            (is (= :bug   (:report/type bug)))
            (is (= :patch (:report/type patch)))
            ;; The :resolves cross-link is still recorded -- only the
            ;; side effects on the bug are gated.
            (is (some #(= "<128@test.org>" (:report/message-id %))
                      (all-related patch))
                ":resolves cross-link is still recorded")
            ;; No auto-credit fired on the bug.
            (is (nil? (:report/acked bug))
                "auto-acked must NOT fire when :patch-triggers? is false")
            (is (nil? (:report/owned bug))
                "auto-owned must NOT fire when :patch-triggers? is false")
            (is (nil? (:report/closed bug))
                "the bug must remain open after the patch arrives")))

        ;; --- Email 40 patch related to bug ---
        (testing "Email 40 patch related to bug"
          (let [patch (get-report db "<40@test.org>")
                bug   (get-report db "<23@test.org>")]
            (is (= :patch (:report/type patch)))
            (is (some #(= "<23@test.org>" (:report/message-id %))
                      (all-related patch)))
            (is (some #(= "<40@test.org>" (:report/message-id %))
                      (all-related bug)))))

        ;; --- Emails 41-42 add then remove maintainer ---
        (testing "Emails 41-42 add then remove maintainer"
          (is (not (common/maintainer? (roles/get-tenures db "direct") "maint2@test.org"))))

        ;; --- Email 44 unignore (Ignore mechanism removed) ---

        ;; --- Email 45 maintainer adds peer ---
        (testing "Email 45 maintainer adds peer"
          (is (common/maintainer? (roles/get-tenures db "direct") "maint3@test.org")))

        ;; --- Email 47 user can't add maintainer ---
        (testing "Email 47 user can't add maintainer"
          (is (not (common/maintainer? (roles/get-tenures db "direct") "user@test.org"))))

        ;; --- Emails 48-49 command with semicolon ---
        (testing "Emails 48-49 command with semicolon"
          (let [r (get-report db "<48@test.org>")]
            (is (= :bug (:report/type r)))
            (is (some? (:report/acked r)))))

        ;; --- Emails 50-51 inline command ignored ---
        (testing "Emails 50-51 inline command ignored"
          (let [r (get-report db "<50@test.org>")]
            (is (= :bug (:report/type r)))
            (is (nil? (:report/closed r)))))

        ;; --- Emails 52-53 Closed. on request ---
        (testing "Emails 52-53 Closed. on request"
          (let [r (get-report db "<52@test.org>")]
            (is (= :request (:report/type r)))
            (is (some? (:report/closed r)))))

        ;; --- Emails 54-55 commands on announcement ---
        (testing "Emails 54-55 commands on announcement"
          (let [r (get-report db "<54@test.org>")]
            (is (= :announcement (:report/type r)))
            (is (nil? (:report/acked r)))
            (is (nil? (:report/owned r)))
            (is (some? (:report/urgent r)))))

        ;; --- Email 59 properly tagged [BUG] ---
        (testing "Email 59 properly tagged [BUG]"
          (is (= :bug (:report/type (get-report db "<59@test.org>")))))

        ;; --- Email 60 [ANNOUNCEMENT] long form ---
        (testing "Email 60 [ANNOUNCEMENT] long form"
          (is (= :announcement (:report/type (get-report db "<60@test.org>")))))

        ;; --- Emails 71-73 permission denials ---
        (testing "Emails 71-73 permission denials"
          (is (not (report-exists? db "<71@test.org>")))
          (is (not (report-exists? db "<72@test.org>")))
          (is (not (report-exists? db "<73@test.org>"))))

        ;; --- Email 61 References-only threading ---
        (testing "Email 61 References-only threading"
          (is (some? (:report/acked (get-report db "<59@test.org>")))))

        ;; --- Email 62 deep thread ---
        (testing "Email 62 deep thread command"
          (let [r (get-report db "<59@test.org>")]
            (is (some? (:report/closed r)))
            (is (>= (count (:report/descendants r)) 2))))

        ;; --- Email 63 orphan ---
        (testing "Email 63 orphan"
          (is (not (report-exists? db "<63@test.org>"))))

        ;; --- Email 64 duplicate tag ---
        (testing "Email 64 [BUG] reply creates own report"
          (is (report-exists? db "<64@test.org>")))

        ;; --- Emails 201-202 malformed labels create no report ---
        (testing "Email 201 lowercase [bug] is rejected"
          (is (not (report-exists? db "<201@test.org>"))))
        (testing "Email 202 glued [BUG]subject is rejected"
          (is (not (report-exists? db "<202@test.org>"))))

        ;; --- Emails 65-66 HTML body fallback ---
        (testing "Emails 65-66 HTML body fallback"
          (let [r (get-report db "<65@test.org>")]
            (is (= :bug (:report/type r)))
            (is (some? (:report/acked r)))))

        ;; --- Email 67 Ignore mechanism removed ---

        ;; --- Emails 68-69 series without cover letter ---
        (testing "Emails 68-69 series without cover letter"
          (let [r68 (get-report db "<68@test.org>")
                r69 (get-report db "<69@test.org>")]
            (is (= :patch (:report/type r68)))
            (is (= "1/2" (:report/patch-seq r68)))
            (is (= "2/2" (:report/patch-seq r69)))
            (is (some? (:report/series r68)))
            (is (some? (:report/series r69)))
            ;; Patches in the same series are related to each other
            (is (some #(= "<69@test.org>" (:report/message-id %))
                      (all-related r68)))
            (is (some #(= "<68@test.org>" (:report/message-id %))
                      (all-related r69)))))

        ;; --- Email 70 different sender ---
        (testing "Email 70 different sender same topic"
          (let [r70 (get-report db "<70@test.org>")
                r68 (get-report db "<68@test.org>")]
            (is (= :patch (:report/type r70)))
            (is (not= (get-in r68 [:report/series :series/id])
                      (get-in r70 [:report/series :series/id])))))

        ;; --- POLL 75 vote variants ---
        (testing "POLL 75 vote format variants"
          (let [r     (get-report db "<75@test.org>")
                votes (get-votes db "<75@test.org>")]
            (is (= :request (:report/type r)))
            (is (= 4 (count votes)))
            (is (= 2 (count (filter #(= :up (:value %)) votes))))
            (is (= 1 (count (filter #(= :down (:value %)) votes))))
            (is (= 1 (count (filter #(= :null (:value %)) votes))))
            (is (not (contains? (set (map :voter votes)) "admin@test.org")))))

        ;; --- Directive unit tests ---
        (testing "detect-lines"
          (is (= [{:action :set :attr :report/acked :email-address "a@b.com" :scope :user :id :acked-by}]
                 (commands/detect-lines :bug "Acked-by: a@b.com\n")))
          (is (= [{:action :set :attr :report/owned :email-address "x@y.com" :scope :user :id :owned-by}]
                 (commands/detect-lines :bug "Owned-by: x@y.com\nIgnore this line\n")))
          (is (= [{:action :unset :attr :report/acked :scope :user :id :unacked}]
                 (commands/detect-lines :bug "Not acked\n")))
          (is (= [{:action :unset :attr :report/urgent :scope :user :id :unurgent}]
                 (commands/detect-lines :bug "Not urgent\n")))
          (is (= [{:action :unset :attr :report/important :scope :user :id :unimportant}]
                 (commands/detect-lines :bug "Not important\n")))
          (is (= [{:action :set-deadline :date (parse-date-iso "2026-06-15") :scope :user :id :deadline}]
                 (commands/detect-lines :bug "Deadline: 2026-06-15\n")))
          (is (= [{:action :unset-deadline :scope :user :id :undeadline}]
                 (commands/detect-lines :bug "No deadline\n")))
          (is (= [{:action :unset-topic :scope :user :id :untopic}]
                 (commands/detect-lines :bug "No topic\n")))
          (is (= [{:action :set-topic :topic "my-topic" :scope :user :id :topic}]
                 (commands/detect-lines :bug "Topic: my-topic\n")))
          (is (= [{:action :set :attr :report/acked :email-address "a@b.com" :scope :user :id :acked-by}
                  {:action :set-deadline :date (parse-date-iso "2026-07-01") :scope :user :id :deadline}
                  {:action :set-topic :topic "urgent-fix" :scope :user :id :topic}]
                 (commands/detect-lines :bug "Acked-by: a@b.com\nDeadline: 2026-07-01\nTopic: urgent-fix\n")))
          (is (= [{:action :set :attr :report/owned :email-address "x@y.com" :scope :user :id :owned-by}]
                 (commands/detect-lines :bug "Thanks for the report.\nOwned-by: x@y.com\nWill look into it.\n")))
          ;; RFC 5322 "Display Name <addr>" format
          (is (= [{:action :set :attr :report/owned :email-address "x@y.com" :scope :user :id :owned-by}]
                 (commands/detect-lines :bug "Owned-by: Some User <x@y.com>\n")))
          ;; Bracketed address without display name: angle brackets must be stripped
          (is (= [{:action :set :attr :report/owned :email-address "x@y.com" :scope :user :id :owned-by}]
                 (commands/detect-lines :bug "Owned-by: <x@y.com>\n")))
          ;; Address must contain a dot in the domain part
          (is (= [] (commands/detect-lines :bug "Owned-by: alice@localhost\n")))
          ;; Address must not contain stray @ characters
          (is (= [] (commands/detect-lines :bug "Owned-by: alice@@host.com\n")))
          (is (= [] (commands/detect-lines :bug "Just a normal reply.\n")))
          (is (nil? (commands/detect-lines :bug nil)))
          ;; Expiry directive
          (is (= [{:action :set-expiry :date (parse-date-iso "2026-09-01") :scope :user :id :expiry}]
                 (commands/detect-lines :bug "Expiry: 2026-09-01\n")))
          (is (= [{:action :unset-expiry :scope :user :id :unexpiry}]
                 (commands/detect-lines :bug "No expiry\n")))
          ;; "Expiry: deadline" is no longer a valid command (use :inactive-after :deadline in config)
          (is (= [] (commands/detect-lines :bug "Expiry: deadline\n")))
          ;; Deadline with duration (relative to email date)
          (let [email-date (parse-date-iso "2026-01-10")
                result (commands/detect-lines :bug "Deadline: 30d\n" nil email-date)]
            (is (= 1 (count result)))
            (is (= :set-deadline (:action (first result))))
            (is (= (parse-date-iso "2026-02-09") (:date (first result)))))
          ;; Expiry with duration (relative to email date)
          (let [email-date (parse-date-iso "2026-01-03")
                result (commands/detect-lines :bug "Expiry: 3d\n" nil email-date)]
            (is (= 1 (count result)))
            (is (= :set-expiry (:action (first result))))
            (is (= (parse-date-iso "2026-01-06") (:date (first result)))))
          ;; Compound duration
          (let [email-date (parse-date-iso "2026-01-01")
                result (commands/detect-lines :bug "Expiry: 1m 2w\n" nil email-date)]
            (is (= 1 (count result)))
            (is (= (parse-date-iso "2026-02-14") (:date (first result)))))
          ;; Expiry now applies to all report types, including announcements
          (let [result (commands/detect-lines :announcement "Expiry: 2026-09-01\n")]
            (is (= 1 (count result)))
            (is (= :set-expiry (:action (first result))))
            (is (= (parse-date-iso "2026-09-01") (:date (first result))))))

        ;; --- resolve-commands unit tests ---
        (testing "resolve-commands"
          (is (= {:set {} :unset #{:report/acked}}
                 (commands/resolve-commands [{:action :set :attr :report/acked :email-address "a@b.com"}
                                             {:action :unset :attr :report/acked}])))
          (is (= {:set {:report/acked "a@b.com"} :unset #{}}
                 (commands/resolve-commands [{:action :unset :attr :report/acked}
                                             {:action :set :attr :report/acked :email-address "a@b.com"}])))
          (is (= {:set {} :unset #{} :undeadline? true}
                 (commands/resolve-commands [{:action :set-deadline :date (parse-date-iso "2026-06-01")}
                                             {:action :unset-deadline}])))
          (is (= {:set {} :unset #{} :deadline (parse-date-iso "2026-06-01")}
                 (commands/resolve-commands [{:action :unset-deadline}
                                             {:action :set-deadline :date (parse-date-iso "2026-06-01")}])))
          (is (= {:set {} :unset #{} :topic "second"}
                 (commands/resolve-commands [{:action :set-topic :topic "first"}
                                             {:action :set-topic :topic "second"}])))
          ;; Expiry resolve
          (is (= {:set {} :unset #{} :expiry (parse-date-iso "2026-09-01")}
                 (commands/resolve-commands [{:action :set-expiry :date (parse-date-iso "2026-09-01")}])))
          (is (= {:set {} :unset #{} :unexpiry? true}
                 (commands/resolve-commands [{:action :set-expiry :date (parse-date-iso "2026-09-01")}
                                             {:action :unset-expiry}])))
          ;; Untopic resolve
          (is (= {:set {} :unset #{} :untopic? true}
                 (commands/resolve-commands [{:action :set-topic :topic "foo"}
                                             {:action :unset-topic}]))))

        ;; --- Bug 81 directives ---
        (testing "Bug 81 Acked-by / Owned-by credit third parties"
          (let [r (get-report db "<81@test.org>")]
            (is (some? (:report/acked r)))
            (is (= "reviewer@test.org" (:report/acked-address r)))
            (is (some? (:report/owned r)))
            (is (= "fixer@test.org" (:report/owned-address r)))))

        ;; --- Bug 81: -by lines are open to any user; the later
        ;; "Not acked" (mail 85) lands on a report already closed by
        ;; mail 84, where state unsets do not apply (only reopening
        ;; and closure relations do), so the ack above survives. ---
        (testing "Bug 81 user Closed-by applies"
          (let [r (get-report db "<81@test.org>")]
            (is (some? (:report/closed r)))
            (is (= "someone@test.org" (:report/closed-address r)))))

        ;; --- Bug 86 last-one-wins ---
        (testing "Bug 86 last-one-wins"
          (is (nil? (:report/acked (get-report db "<86@test.org>")))))

        ;; --- Bug 88 Closed-by ---
        (testing "Bug 88 Closed-by"
          (let [r (get-report db "<88@test.org>")]
            (is (some? (:report/closed r)))
            (is (= :resolved (:report/close-reason r)))
            (is (= "closer@test.org" (:report/closed-address r)))))

        ;; --- Bug 90 trigger + directive ---
        (testing "Bug 90 Confirmed trigger + Owned-by directive"
          (let [r (get-report db "<90@test.org>")]
            (is (some? (:report/acked r)))
            (is (some? (:report/owned r)))
            (is (= "fixer@test.org" (:report/owned-address r)))))

        ;; --- Bug 90 Fixed + Unclosed ---
        (testing "Bug 90 Fixed + Unclosed (directive wins)"
          (is (nil? (:report/closed (get-report db "<90@test.org>")))))

        ;; --- Bug 93 Confirmed ---
        (testing "Bug 93 Confirmed"
          (let [r (get-report db "<93@test.org>")]
            (is (some? (:report/acked r)))))

        ;; --- Bug 90 Topic directive ---
        (testing "Bug 90 Topic directive"
          (is (= "regression" (:report/topic-value (get-report db "<90@test.org>")))))

        ;; --- Bug 93 Undeadline ---
        (testing "Bug 93 Undeadline removes deadline"
          (is (nil? (:report/deadline-value (get-report db "<93@test.org>")))))

        ;; --- Bug 98 standalone deadline ---
        (testing "Bug 98 standalone deadline"
          (is (some? (:report/deadline-value (get-report db "<98@test.org>")))))

        ;; --- Email 100 [source-name] prefix (bone-source fallback removed) ---
        (testing "Email 100 -- no bone-source fallback"
          (is (not (report-exists? db "<100@test.org>"))))

        ;; --- CHG 102+103 / REL 104 ---
        (testing "CHG 102+103 / REL 104 release closes multiple changes"
          (let [chg1 (get-report db "<102@test.org>")
                chg2 (get-report db "<103@test.org>")
                rel  (get-report db "<104@test.org>")
                rel-mids (set (map :report/message-id (all-related rel)))]
            (is (some? (:report/closed chg1)))
            (is (some? (:report/closed chg2)))
            (is (= :resolved (:report/close-reason chg1)))
            (is (= :resolved (:report/close-reason chg2)))
            (is (contains? rel-mids "<102@test.org>"))
            (is (contains? rel-mids "<103@test.org>"))
            (is (some #(= "<104@test.org>" (:report/message-id %))
                      (all-related chg1)))
            (is (some #(= "<104@test.org>" (:report/message-id %))
                      (all-related chg2)))))

        ;; --- Emails 105-107 Superseded-by ---
        (testing "Bug 105 superseded by 106"
          (let [r105 (get-report db "<105@test.org>")
                r106 (get-report db "<106@test.org>")]
            (is (some? (:report/closed r105)) "superseded report is closed")
            (is (= :superseded (:report/close-reason r105)))
            (is (= "<106@test.org>"
                    (some-> (superseded-by-target r105) :report/message-id)))
            ;; Bidirectional related link
            (is (some #(= "<106@test.org>" (:report/message-id %))
                      (all-related r105)))
            (is (some #(= "<105@test.org>" (:report/message-id %))
                      (all-related r106)))))

        ;; --- Emails 108-110 Supersede then unsupersede ---
        ;; 109 admin supersedes 108 by 106, 110 admin "Not superseded-by"
        ;; reopens it.  Email 111 is a neutral user reply (no command),
        ;; so the final state of 108 is cleanly "reopened".
        (testing "Bug 108 superseded then unsuperseded"
          (let [r108 (get-report db "<108@test.org>")
                r106 (get-report db "<106@test.org>")]
            (is (nil? (:report/closed r108)) "unsuperseded report is reopened")
            (is (nil? (:report/close-reason r108)))
            (is (nil? (superseded-by-target r108)))
            ;; Related link from the supersede is removed
            (is (not (some #(= "<106@test.org>" (:report/message-id %))
                           (all-related r108))))
            (is (not (some #(= "<108@test.org>" (:report/message-id %))
                           (all-related r106))))))

        ;; --- Emails 122-124 user Superseded-by ---
        ;; user-a (not a maintainer) marks their own narrow report 122
        ;; as superseded by user-b's broader report 123.  With
        ;; :superseded-by scoped as :user, this must succeed.
        (testing "Bug 122 superseded by 123 via regular user"
          (let [r122 (get-report db "<122@test.org>")
                r123 (get-report db "<123@test.org>")]
            (is (some? (:report/closed r122)) "user supersede closes 122")
            (is (= :superseded (:report/close-reason r122)))
            (is (= "<123@test.org>"
                    (some-> (superseded-by-target r122) :report/message-id)))
            ;; Bidirectional related link set by the supersede
            (is (some #(= "<123@test.org>" (:report/message-id %))
                      (all-related r122)))
            (is (some #(= "<122@test.org>" (:report/message-id %))
                      (all-related r123)))))

        ;; --- Emails 114-116 Not closed on superseded report ---
        (testing "Bug 114 superseded then reopened via Not closed"
          (let [r114 (get-report db "<114@test.org>")
                r106 (get-report db "<106@test.org>")]
            (is (nil? (:report/closed r114)) "report is reopened")
            (is (nil? (:report/close-reason r114)) "close-reason is cleared")
            (is (nil? (superseded-by-target r114)) "superseded-by-target is cleared")
            (is (not (some #(= "<106@test.org>" (:report/message-id %))
                           (all-related r114)))
                "related link to superseder is removed")
            (is (not (some #(= "<114@test.org>" (:report/message-id %))
                           (all-related r106)))
                "reverse related link is removed")))

        ;; --- Directive unit tests for supersede ---
        (testing "detect-lines: Superseded-by with angle brackets"
          (is (= [{:action :set-superseded :attr :rel/supersedes :target-message-id "<msg@example.com>" :scope :user :id :superseded-by}]
                 (commands/detect-lines :bug "Superseded-by: <msg@example.com>\n"))))

        (testing "detect-lines: Superseded-by tolerates an URL prefix"
          (is (= [{:action :set-superseded :attr :rel/supersedes :target-message-id "<msg@example.com>" :scope :user :id :superseded-by}]
                 (commands/detect-lines :bug "Superseded-by: https://orgmode.org/list/<msg@example.com>\n"))))

        (testing "detect-lines: Superseded-by accepts a public-inbox URL"
          (is (= [{:action :set-superseded :attr :rel/supersedes :target-message-id "<msg@example.com>" :scope :user :id :superseded-by}]
                 (commands/detect-lines :bug "Superseded-by: https://list.orgmode.org/orgmode/msg@example.com/\n")))
          (is (= [{:action :set-superseded :attr :rel/supersedes :target-message-id "<msg@example.com>" :scope :user :id :superseded-by}]
                 (commands/detect-lines :bug "Superseded-by: https://list.orgmode.org/orgmode/msg@example.com\n"))))

        (testing "detect-lines: Superseded-by accepts a bare message-id"
          (is (= [{:action :set-superseded :attr :rel/supersedes :target-message-id "<msg@example.com>" :scope :user :id :superseded-by}]
                 (commands/detect-lines :bug "Superseded-by: msg@example.com\n"))))

        (testing "detect-lines: Superseded-by rejects URLs where the @ segment is non-terminal"
          (is (= [] (commands/detect-lines :bug "Superseded-by: https://example.com/foo@bar/baz.html\n"))))

        (testing "detect-lines: Not superseded-by"
          (is (= [{:action :unset-superseded :attr :rel/supersedes-from :scope :user :id :unsuperseded-by :target-message-id "<msg@example.com>"}]
                 (commands/detect-lines :bug "Not superseded-by: <msg@example.com>\n"))))

        (testing "detect-lines: Supersedes (symmetric of Superseded-by)"
          (is (= [{:action :set-supersedes :attr :rel/supersedes :target-message-id "<msg@example.com>" :scope :user :id :supersedes}]
                 (commands/detect-lines :bug "Supersedes: <msg@example.com>\n"))))

        (testing "detect-lines: Not supersedes"
          (is (= [{:action :unset-supersedes :attr :rel/supersedes-to :scope :user :id :unsupersedes :target-message-id "<msg@example.com>"}]
                 (commands/detect-lines :bug "Not supersedes: <msg@example.com>\n"))))

        (testing "resolve-commands: superseded-by"
          (is (= {:set {} :unset #{} :superseded-by "<mid@host>"}
                 (commands/resolve-commands
                  [{:action :set-superseded :target-message-id "<mid@host>"}]))))

        (testing "resolve-commands: unsuperseded"
          (is (= {:set {} :unset #{} :unsuperseded-by? true :unsuperseded-by-mid "<mid@host>"}
                 (commands/resolve-commands
                  [{:action :unset-superseded :target-message-id "<mid@host>"}]))))

        (testing "resolve-commands: supersede then unsupersede"
          (is (= {:set {} :unset #{} :unsuperseded-by? true :unsuperseded-by-mid "<mid@host>"}
                 (commands/resolve-commands
                  [{:action :set-superseded :target-message-id "<mid@host>"}
                   {:action :unset-superseded :target-message-id "<mid@host>"}]))))

        (testing "resolve-commands: supersedes"
          (is (= {:set {} :unset #{} :supersedes "<mid@host>"}
                 (commands/resolve-commands
                  [{:action :set-supersedes :target-message-id "<mid@host>"}]))))

        (testing "resolve-commands: supersedes then not superseding"
          (is (= {:set {} :unset #{} :unsupersedes? true :unsupersedes-mid "<mid@host>"}
                 (commands/resolve-commands
                  [{:action :set-supersedes :target-message-id "<mid@host>"}
                   {:action :unset-supersedes :target-message-id "<mid@host>"}]))))

        ;; --- Emails 130-131 Supersedes (close target) ---
        (testing "Bug 130 closed by 131 via Supersedes:"
          (let [r130 (get-report db "<130@test.org>")
                r131 (get-report db "<131@test.org>")]
            (is (some? (:report/closed r130)) "target of Supersedes: is closed")
            (is (= :superseded (:report/close-reason r130)))
            (is (nil? (:report/closed r131)) "the directive's report stays open")
            ;; The canonical relation has :rel/from = 130 (closed) and
            ;; :rel/to = 131 (replacement), as if 131 had received a
            ;; Superseded-by: <131> directly.
            (is (= "<131@test.org>"
                   (some-> (superseded-by-target r130) :report/message-id)))
            (is (some #(= "<131@test.org>" (:report/message-id %))
                      (all-related r130)))
            (is (some #(= "<130@test.org>" (:report/message-id %))
                      (all-related r131)))))

        ;; --- Emails 132-134 Supersedes then Not supersedes ---
        (testing "Bug 132 superseded by 133 then reopened via Not supersedes"
          (let [r132 (get-report db "<132@test.org>")
                r133 (get-report db "<133@test.org>")]
            (is (nil? (:report/closed r132)) "Not supersedes reopens the previously-closed target")
            (is (nil? (:report/close-reason r132)))
            (is (nil? (superseded-by-target r132)))
            (is (not (some #(= "<133@test.org>" (:report/message-id %))
                           (all-related r132)))
                "Not supersedes retracts the :related-to companion too")
            (is (nil? (:report/closed r133)) "the superseder stays open throughout")))

        ;; --- Emails 135-137 Cycle flip via cross-directive ---
        (testing "Bug 135 superseded then unsuperseded by 136 flip; 136 closed instead"
          (let [r135 (get-report db "<135@test.org>")
                r136 (get-report db "<136@test.org>")]
            (is (nil? (:report/closed r135)) "135 reopens once 136 flips the direction")
            (is (nil? (:report/close-reason r135)))
            (is (some? (:report/closed r136)) "136 is now closed in the flipped direction")
            (is (= :superseded (:report/close-reason r136)))
            (is (= "<135@test.org>"
                   (some-> (superseded-by-target r136) :report/message-id)))
            (is (some #(= "<135@test.org>" (:report/message-id %))
                      (all-related r136)))))

        ;; ---------------------------------------------------------------
        ;; Participant / contributor tracking
        ;; ---------------------------------------------------------------

        (testing "Bug reporter is a participant without contributor-since"
          (let [p (get-participant db "direct" "user@test.org")]
            (is (some? p) "user@test.org should be a participant")
            (is (some? (:participant/since p)))
            (is (some? (:participant/contributor-since p))
                "user@test.org also submitted patches, so contributor-since must be set")))

        (testing "Patch submitter gets contributor-since stamped"
          ;; user@test.org submitted [PATCH org-agenda 1/2] Fix sorting (<07@test.org>)
          (let [p (get-participant db "direct" "user@test.org")]
            (is (some? (:participant/contributor-since p))
                "contributor-since should be set for a patch submitter")
            (is (inst? (:participant/contributor-since p)))))

        (testing "Command applier becomes a participant"
          ;; maint@test.org applies Confirmed/Urgent/Fixed on bug <02@test.org>
          (let [p (get-participant db "direct" "maint@test.org")]
            (is (some? p) "maint@test.org should be a participant via commands")))

        (testing "Command-only participant has no contributor-since"
          ;; newadmin@test.org only applied "Closed." on <53@test.org>
          (let [p (get-participant db "direct" "newadmin@test.org")]
            (is (some? p) "newadmin@test.org should be a participant via Closed command")
            (is (nil? (:participant/contributor-since p))
                "newadmin@test.org never submitted a patch")))

        (testing "Participant entity is unique per source+email (idempotent)"
          (let [count (d/q '[:find (count ?e) .
                             :where
                             [?e :participant/email "user@test.org"]
                             [?e :participant/source "direct"]]
                           db)]
            (is (= 1 count)
                "Multiple reports/patches from same person should not create duplicates")))

        (testing "Participant on public-list source is separate from direct"
          ;; <30@test.org> is a bug on public-list by user@test.org
          (let [p (get-participant db "public-list" "user@test.org")]
            (is (some? p) "user@test.org should also be a participant on public-list")))

        ;; --- Emails 117-119: Same-subject patch supersession ---
        (testing "Patch 117 superseded by same-subject reply 118"
          (let [r117 (get-report db "<117@test.org>")]
            (is (= :patch (:report/type r117)))
            (is (some? (:report/closed r117)) "first patch should be closed")
            (is (= :superseded (:report/close-reason r117)))
            (is (= "<118@test.org>"
                    (some-> (superseded-by-target r117) :report/message-id)))
            (is (nil? (:report/acked r117))
                "v2 reply must not spuriously credit v1 patch as acked")
            (is (nil? (:report/owned r117))
                "v2 reply must not spuriously credit v1 patch as owned")))

        (testing "Patch 118 superseded by same-subject reply 119"
          (let [r118 (get-report db "<118@test.org>")]
            (is (= :patch (:report/type r118)))
            (is (some? (:report/closed r118)) "second patch should be closed")
            (is (= :superseded (:report/close-reason r118)))
            (is (= "<119@test.org>"
                    (some-> (superseded-by-target r118) :report/message-id)))))

        (testing "Patch 119 is open (latest in chain)"
          (let [r119 (get-report db "<119@test.org>")]
            (is (= :patch (:report/type r119)))
            (is (nil? (:report/closed r119)) "latest patch should remain open")))

        ;; --- Emails 120-121: Inline diff supersession (no [PATCH] tag) ---
        (testing "Inline diff 120 superseded by same-subject reply 121"
          (let [r120 (get-report db "<120@test.org>")]
            (is (= :patch (:report/type r120)))
            (is (some? (:report/closed r120)) "first diff should be closed")
            (is (= :superseded (:report/close-reason r120)))
            (is (= "<121@test.org>"
                    (some-> (superseded-by-target r120) :report/message-id)))))

        (testing "Inline diff 121 is open (latest)"
          (let [r121 (get-report db "<121@test.org>")]
            (is (= :patch (:report/type r121)))
            (is (nil? (:report/closed r121)) "latest diff should remain open")))

        ;; --- Email 203: [BUG] + .patch attachment ---
        (testing "Bug 203 stores attached patch as metadata"
          (let [r203 (get-report db "<203@test.org>")]
            (is (= :bug (:report/type r203))
                "label decides: [BUG] + .patch attachment is a :bug")
            (is (nil? (:report/acked r203))
                "a root [BUG]+.patch must NOT self-credit its reporter")
            (is (nil? (:report/owned r203))
                "the implicit Acked/Owned only fires on replies, not on the root")
            (let [patches (:report/patches r203)]
              (is (= 1 (count patches))
                  ":report/patches is filled on the bug")
              (let [p (first patches)]
                (is (= "fix-save-crash.patch" (:patch/filename p)))
                (is (= :attachment (:patch/source p)))
                (is (= "User A <user-a@test.org>" (:patch/author p))
                    "format-patch headers are parsed and stored")
                (is (= "Fix crash on save" (:patch/subject p)))))))

        ;; --- Emails 204-205: implicit Acked/Owned from patch reply ---
        (testing "Bug 204 auto-credited by patch reply 205"
          (let [r204 (get-report db "<204@test.org>")]
            (is (= :bug (:report/type r204)))
            (is (some? (:report/acked r204))
                "reply with .patch attachment implicitly credits acked")
            (is (some? (:report/owned r204))
                "reply with .patch attachment implicitly credits owned")
            (is (= "fixer@test.org" (:report/acked-address r204)))
            (is (= "fixer@test.org" (:report/owned-address r204)))))

        ;; --- Emails 206-207: garde-fou A preserves manual setter ---
        (testing "Bug 206 keeps manual owner despite implicit signal in 207"
          (let [r206 (get-report db "<206@test.org>")]
            (is (= :bug (:report/type r206)))
            (is (= "admin@test.org" (:report/owned-address r206))
                "garde-fou A: pre-existing Owned-by setter is preserved")))

        ;; --- Emails 208-209: :patch-triggers? false suppresses implicit ---
        (testing "Bug 208 on no-triggers source skips implicit Acked/Owned"
          (let [r208 (get-report db "<208@test.org>")]
            (is (= :bug (:report/type r208)))
            (is (nil? (:report/acked r208))
                ":patch-triggers? false gates the implicit trigger")
            (is (nil? (:report/owned r208)))))

        ;; --- Emails 211-213: Acked requires a distinct second party ---
        (testing "Self-ack ignored, third-party ack succeeds; self-owned allowed"
          ;; Alice (212) tries to Acked + Owned her own bug.  Bob (213)
          ;; later writes "Confirmed".  Alice's self-Acked was dropped,
          ;; so the acked slot was free for Bob.  Alice's self-Owned
          ;; stuck (self-ownership is allowed).
          (let [r211 (get-report db "<211@test.org>")]
            (is (= :bug (:report/type r211)))
            (is (= "bob@test.org" (:report/acked-address r211))
                "Bob (third party) acks -- Alice's self-ack was dropped")
            (is (= "alice@test.org" (:report/owned-address r211))
                "Alice owns her own bug -- self-ownership is allowed")))

        ;; --- Emails 214-215: implicit Acked from patch reply is also gated ---
        (testing "Reporter sending a patch in reply owns but does not ack"
          (let [r214 (get-report db "<214@test.org>")]
            (is (= :bug (:report/type r214)))
            (is (nil? (:report/acked r214))
                "implicit ack is dropped when the patch sender is the reporter")
            (is (= "carol@test.org" (:report/owned-address r214))
                "implicit owned still fires -- self-ownership is allowed")))

        ;; --- Emails 216-217: a reply targets the nearest report,
        ;; even when the reply itself creates a new report. ---
        (testing "Reply that creates v2 patch hands its commands to v1 (nearest)"
          (let [r216 (get-report db "<216@test.org>")
                r217 (get-report db "<217@test.org>")]
            (is (= :patch (:report/type r216)))
            (is (= :patch (:report/type r217)))
            (is (= "lexer" (:report/topic-value r216))
                "Topic directive lands on v1 (nearest), overriding the label topic")
            (is (some? (:report/important r216))
                "Important trigger lands on v1 (nearest), not on the new v2")
            (is (= "parser" (:report/topic-value r217))
                "v2 keeps the topic from its own [PATCH parser] label")
            (is (nil? (:report/important r217))
                "v2 carries no directive: the body annotates the thread, not the carrier"))))
      (finally
        (teardown! ctx)))))

;; ---------------------------------------------------------------------------
;; Pending-thread (out-of-order delivery rescue)
;; ---------------------------------------------------------------------------

(defn- mk-email
  "Build a minimal email entity map for pending-thread tests."
  [{:keys [mid subject from date in-reply-to refs body]}]
  (let [refs-vec    (vec refs)
        ancestor    (vec (distinct (cond-> refs-vec
                                     (and in-reply-to (not (some #{in-reply-to} refs-vec)))
                                     (conj in-reply-to))))]
    (cond-> {:email/message-id      mid
             :email/message-id-hash (common/mid-hash mid)
             :email/subject         subject
             :email/from-address    from
             :email/author-address  from
             :email/date-sent       date
             :email/ingested-at     date
             :email/body-text       (or body "")}
      in-reply-to        (assoc :email/in-reply-to in-reply-to)
      (seq refs)         (assoc :email/references (str/join " " refs))
      (seq ancestor)     (assoc :email/ancestor-mid-hashes
                                (mapv common/mid-hash ancestor)))))

(defn- store-and-process!
  "Insert an email entity, then run digest/process-email! on it."
  [conn email-map source-name]
  (let [email-with-source (assoc email-map :email/source source-name)]
    (d/transact! conn [email-with-source])
    (let [eid (lookup/email-eid (d/db conn) (:email/message-id email-map))
          email (d/pull (d/db conn) digest/email-pull-pattern eid)]
      (digest/process-email! conn source-map sources email))))

(defn- pending? [db mid]
  (boolean
   (when-let [e (lookup/email-eid db mid)]
     (:email/pending-thread? (d/pull db [:email/pending-thread?] e)))))

(deftest pending-thread-no-irt-not-flagged
  (testing "An email with no In-Reply-To is never flagged pending."
    (let [{:keys [conn] :as ctx} (setup-db!)]
      (try
        (store-and-process! conn
                            (mk-email {:mid "<root-1@test.org>"
                                       :subject "[BUG] something"
                                       :from "user@test.org"
                                       :date #inst "2026-05-01T10:00:00"
                                       :body "broken\n"})
                            "direct")
        (let [db (d/db conn)]
          (is (false? (pending? db "<root-1@test.org>"))
              "Root email should not be pending")
          (is (report-exists? db "<root-1@test.org>")
              "Bug report should be created"))
        (finally
          (teardown! ctx))))))

(deftest pending-thread-in-order-typical
  (testing "In-order : reply arrives after parent => no pending flag, command applied."
    (let [{:keys [conn] :as ctx} (setup-db!)]
      (try
        (store-and-process! conn
                            (mk-email {:mid "<inord-1@test.org>"
                                       :subject "[BUG] crash"
                                       :from "user@test.org"
                                       :date #inst "2026-05-01T10:00:00"
                                       :body "broken\n"})
                            "direct")
        (store-and-process! conn
                            (mk-email {:mid "<inord-2@test.org>"
                                       :subject "Re: [BUG] crash"
                                       :from "admin@test.org"
                                       :date #inst "2026-05-01T11:00:00"
                                       :in-reply-to "<inord-1@test.org>"
                                       :body "Closed.\n"})
                            "direct")
        (let [db (d/db conn)
              r  (get-report db "<inord-1@test.org>")]
          (is (false? (pending? db "<inord-2@test.org>"))
              "Reply with resolved IRT should not be pending")
          (is (some? (:report/closed r)) "Bug should be closed by the reply"))
        (finally
          (teardown! ctx))))))

(deftest pending-thread-out-of-order-rescue
  (testing "Out-of-order : reply ingested before parent => pending, then retried."
    (let [{:keys [conn] :as ctx} (setup-db!)]
      (try
        ;; 1. Reply arrives first, parent missing => flagged pending,
        ;;    command not applied yet.
        (store-and-process! conn
                            (mk-email {:mid "<ooo-reply@test.org>"
                                       :subject "Re: [BUG] crash"
                                       :from "admin@test.org"
                                       :date #inst "2026-05-01T11:00:00"
                                       :in-reply-to "<ooo-bug@test.org>"
                                       :body "Closed.\n"})
                            "direct")
        (let [db (d/db conn)]
          (is (true? (pending? db "<ooo-reply@test.org>"))
              "Reply should be flagged pending while IRT is absent")
          (is (not (report-exists? db "<ooo-bug@test.org>"))
              "No report yet for the missing parent"))

        ;; 2. Parent arrives => its post-process retry hook should re-process
        ;;    the pending reply and apply the command.
        (store-and-process! conn
                            (mk-email {:mid "<ooo-bug@test.org>"
                                       :subject "[BUG] crash"
                                       :from "user@test.org"
                                       :date #inst "2026-05-01T10:00:00"
                                       :body "broken\n"})
                            "direct")
        (let [db (d/db conn)
              r  (get-report db "<ooo-bug@test.org>")]
          (is (false? (pending? db "<ooo-reply@test.org>"))
              "Pending flag must be cleared after retry")
          (is (some? (:report/closed r))
              "Bug should now be closed via the retried 'Closed.' trigger"))
        (finally
          (teardown! ctx))))))

(deftest carrier-dispatch-supersedes-applies-to-new-report
  (testing "A reply that creates a new report AND carries Supersedes:
            <external-mid> applies the relation to the new report
            (closes the external target), not to the thread parent."
    (let [{:keys [conn] :as ctx} (setup-db!)]
      (try
        ;; 1. Old bug (will be superseded).
        (store-and-process! conn
                            (mk-email {:mid "<old-bug@test.org>"
                                       :subject "[BUG] narrow framing"
                                       :from "alice@test.org"
                                       :date #inst "2026-05-01T10:00:00"
                                       :body "narrow report\n"})
                            "direct")
        ;; 2. Unrelated parent thread (will be the In-Reply-To target).
        (store-and-process! conn
                            (mk-email {:mid "<thread-root@test.org>"
                                       :subject "[BUG] something else"
                                       :from "bob@test.org"
                                       :date #inst "2026-05-01T11:00:00"
                                       :body "another bug\n"})
                            "direct")
        ;; 3. A reply that itself opens a new [BUG] and supersedes the
        ;;    old bug from step 1.  Before the carrier loosening the
        ;;    Supersedes: would have been dispatched to <thread-root>
        ;;    (the nearest report in the thread), where it would have
        ;;    no useful meaning.  With the loosening it lands on the
        ;;    new report and closes <old-bug>.
        (store-and-process! conn
                            (mk-email {:mid "<new-bug@test.org>"
                                       :subject "[BUG] broader framing"
                                       :from "carol@test.org"
                                       :date #inst "2026-05-01T12:00:00"
                                       :in-reply-to "<thread-root@test.org>"
                                       :body "Supersedes: <old-bug@test.org>\n"})
                            "direct")
        (let [db (d/db conn)
              old   (get-report db "<old-bug@test.org>")
              new   (get-report db "<new-bug@test.org>")
              root  (get-report db "<thread-root@test.org>")]
          (is (some? new)        "new report was created")
          (is (= :bug (:report/type new)))
          (is (some? (:report/closed old))
              "old bug was closed via Supersedes: from the new report's body")
          (is (= :superseded (:report/close-reason old)))
          (is (nil? (:report/closed root))
              "the thread parent (unrelated to the supersede) is untouched"))
        (finally
          (teardown! ctx))))))

(deftest oversized-mid-thread-and-close
  (testing "A report whose Message-ID far exceeds the LMDB key limit is
            stored, threaded, and closable: every mid-keyed lookup goes
            through the fixed-length hash (regression: MDB_BAD_VALSIZE
            used to lose the closing reply)."
    (let [{:keys [conn] :as ctx} (setup-db!)
          long-mid (str "<" (apply str (repeat 400 "a")) "@x>")]
      (try
        (store-and-process! conn
                            (mk-email {:mid long-mid
                                       :subject "[BUG] pathological mid"
                                       :from "alice@test.org"
                                       :date #inst "2026-05-01T10:00:00"
                                       :body "broken\n"})
                            "direct")
        (is (report-exists? (d/db conn) long-mid)
            "report created under an oversized mid")
        (store-and-process! conn
                            (mk-email {:mid "<closer@test.org>"
                                       :subject "Re: [BUG] pathological mid"
                                       :from "alice@test.org"
                                       :date #inst "2026-05-02T10:00:00"
                                       :in-reply-to long-mid
                                       :body "Fixed.\n"})
                            "direct")
        (let [r (get-report (d/db conn) long-mid)]
          (is (some? (:report/closed r))
              "closing reply resolved the report across the oversized mid")
          (is (= :resolved (:report/close-reason r))))
        (finally
          (teardown! ctx))))))

(deftest reply-to-absent-parent-defers-commands-until-rescue
  (testing "A reply that creates a report while its (long-mid) parent is
            absent from the DB goes pending: its body commands are
            deferred, then applied when the parent arrives and the
            rescue re-processes the reply."
    (let [{:keys [conn] :as ctx} (setup-db!)
          parent-mid (str "<" (apply str (repeat 250 "p")) "@x>")]
      (try
        (store-and-process! conn
                            (mk-email {:mid "<deferred-bug@test.org>"
                                       :subject "[BUG] deferred flags"
                                       :from "alice@test.org"
                                       :date #inst "2026-05-02T10:00:00"
                                       :in-reply-to parent-mid
                                       :body "Important.\n"})
                            "direct")
        (let [r (get-report (d/db conn) "<deferred-bug@test.org>")]
          (is (some? r) "report is created in Phase 2 even when pending")
          (is (nil? (:report/important r)) "commands deferred while pending"))
        (is (true? (pending? (d/db conn) "<deferred-bug@test.org>")))
        ;; The parent arrives late: the rescue re-processes the reply.
        (store-and-process! conn
                            (mk-email {:mid parent-mid
                                       :subject "long-mid thread root"
                                       :from "bob@test.org"
                                       :date #inst "2026-05-01T10:00:00"
                                       :body "root\n"})
                            "direct")
        (is (false? (pending? (d/db conn) "<deferred-bug@test.org>"))
            "reply rescued once the parent is stored")
        (is (some? (:report/important
                    (get-report (d/db conn) "<deferred-bug@test.org>")))
            "deferred Important. applied after the rescue")
        (finally
          (teardown! ctx))))))

(deftest pending-thread-references-anchor
  (testing "Reply with missing IRT but a known References ancestor is threaded immediately."
    (let [{:keys [conn] :as ctx} (setup-db!)]
      (try
        ;; 1. Root bug, ingested first => creates a report.
        (store-and-process! conn
                            (mk-email {:mid "<shared-bug@test.org>"
                                       :subject "[BUG] flaky"
                                       :from "user@test.org"
                                       :date #inst "2026-05-01T10:00:00"
                                       :body "broken\n"})
                            "direct")
        ;; 2. Reply whose IRT points to a missing intermediate, but
        ;;    References include the root => no longer pending: the
        ;;    References ancestor anchors the thread.
        (store-and-process! conn
                            (mk-email {:mid "<shared-reply@test.org>"
                                       :subject "Re: [BUG] flaky"
                                       :from "admin@test.org"
                                       :date #inst "2026-05-01T12:00:00"
                                       :in-reply-to "<shared-mid@test.org>"
                                       :refs ["<shared-bug@test.org>"]
                                       :body "Closed.\n"})
                            "direct")
        (let [db (d/db conn)
              r  (get-report db "<shared-bug@test.org>")]
          (is (false? (pending? db "<shared-reply@test.org>"))
              "Reply must not be pending -- References ancestor is in DB")
          (is (some? (:report/closed r))
              "Root bug must be closed by the Closed. trigger right away"))
        (finally
          (teardown! ctx))))))

(deftest pending-thread-splice-through-stored
  (testing "thread-lookup splices through a stored pending intermediate to reach the report."
    (let [{:keys [conn] :as ctx} (setup-db!)]
      (try
        ;; 1. Root bug => report exists.
        (store-and-process! conn
                            (mk-email {:mid "<spl-bug@test.org>"
                                       :subject "[BUG] flaky"
                                       :from "user@test.org"
                                       :date #inst "2026-05-01T10:00:00"
                                       :body "broken\n"})
                            "direct")
        ;; 2. An intermediate reply arrives BUT its own IRT points to
        ;;    a missing message (and no References) => flagged pending,
        ;;    *not* attached as a descendant of the bug report.
        (store-and-process! conn
                            (mk-email {:mid "<spl-mid@test.org>"
                                       :subject "Re: [BUG] flaky"
                                       :from "admin@test.org"
                                       :date #inst "2026-05-01T11:00:00"
                                       :in-reply-to "<spl-missing@test.org>"
                                       :body "tracking\n"})
                            "direct")
        ;; Sanity check: intermediate is pending and not attached.
        (is (true? (pending? (d/db conn) "<spl-mid@test.org>"))
            "Setup: intermediate must be pending (its IRT is unknown)")
        ;; 3. Final reply with IRT = the stored pending intermediate
        ;;    (no References). With splicing, thread-lookup walks
        ;;    through the pending intermediate's own ancestor mids and
        ;;    reaches the bug report -- so Closed. applies.
        (store-and-process! conn
                            (mk-email {:mid "<spl-final@test.org>"
                                       :subject "Re: [BUG] flaky"
                                       :from "admin@test.org"
                                       :date #inst "2026-05-01T12:00:00"
                                       :in-reply-to "<spl-mid@test.org>"
                                       :refs ["<spl-bug@test.org>"]
                                       :body "Closed.\n"})
                            "direct")
        (let [db (d/db conn)
              r  (get-report db "<spl-bug@test.org>")]
          (is (false? (pending? db "<spl-final@test.org>"))
              "Final reply not pending -- the stored pending intermediate anchors it")
          (is (some? (:report/closed r))
              "Closed. must reach the bug via splicing through the pending intermediate"))
        (finally
          (teardown! ctx))))))

(deftest pending-thread-splice-no-references
  (testing "Splicing also works when the final reply has no References at all."
    ;; This is the PavoDive case: a late reply whose only ancestor mid
    ;; is the immediate parent, and that parent itself is stored but
    ;; pending (broken by a missing IRT of its own).
    (let [{:keys [conn] :as ctx} (setup-db!)]
      (try
        (store-and-process! conn
                            (mk-email {:mid "<nr-bug@test.org>"
                                       :subject "[BUG] flaky"
                                       :from "user@test.org"
                                       :date #inst "2026-05-01T10:00:00"
                                       :body "broken\n"})
                            "direct")
        (store-and-process! conn
                            (mk-email {:mid "<nr-mid@test.org>"
                                       :subject "Re: [BUG] flaky"
                                       :from "admin@test.org"
                                       :date #inst "2026-05-01T11:00:00"
                                       :in-reply-to "<nr-missing@test.org>"
                                       :refs ["<nr-bug@test.org>"
                                              "<nr-missing@test.org>"]
                                       :body "tracking\n"})
                            "direct")
        ;; Intermediate has a known References ancestor (the bug), so
        ;; with (1) it's NOT pending and is attached as a descendant.
        (is (false? (pending? (d/db conn) "<nr-mid@test.org>"))
            "Intermediate must be threaded via its References anchor")
        ;; Final reply with NO References, IRT to the intermediate.
        (store-and-process! conn
                            (mk-email {:mid "<nr-final@test.org>"
                                       :subject "Re: [BUG] flaky"
                                       :from "admin@test.org"
                                       :date #inst "2026-05-01T12:00:00"
                                       :in-reply-to "<nr-mid@test.org>"
                                       :body "Closed.\n"})
                            "direct")
        (let [db (d/db conn)
              r  (get-report db "<nr-bug@test.org>")]
          (is (false? (pending? db "<nr-final@test.org>"))
              "Final reply is anchored via its IRT (stored intermediate)")
          (is (some? (:report/closed r))
              "Bug closed via the chain: final -> intermediate -> bug"))
        (finally
          (teardown! ctx))))))

(deftest pending-thread-ttl-flush
  (testing "TTL flush forces processing of stale pending emails."
    (let [{:keys [conn] :as ctx} (setup-db!)]
      (try
        ;; Create a root report so threading has something to attach to.
        (store-and-process! conn
                            (mk-email {:mid "<ttl-bug@test.org>"
                                       :subject "[BUG] orphan"
                                       :from "user@test.org"
                                       :date #inst "2026-04-01T10:00:00"
                                       :body "broken\n"})
                            "direct")
        ;; Reply whose IRT target never arrives -- manually backdate
        ;; ingested-at to simulate an old pending email.
        (let [old-date #inst "2026-04-01T11:00:00"]
          (d/transact! conn
                       [(assoc (mk-email {:mid "<ttl-orphan@test.org>"
                                          :subject "Re: [BUG] orphan"
                                          :from "admin@test.org"
                                          :date old-date
                                          :in-reply-to "<never-arrives@test.org>"
                                          :refs ["<ttl-bug@test.org>"]
                                          :body "Closed.\n"})
                               :email/source "direct"
                               :email/pending-thread? true
                               :email/ingested-at old-date)]))
        (is (true? (pending? (d/db conn) "<ttl-orphan@test.org>"))
            "Setup: orphan should be pending")

        ;; TTL flush with a 1-day window -- orphan is older than 30 days.
        (digest/flush-stale-pending! conn source-map sources 1)

        (let [db (d/db conn)
              r  (get-report db "<ttl-bug@test.org>")]
          (is (false? (pending? db "<ttl-orphan@test.org>"))
              "Pending flag cleared by TTL flush")
          (is (some? (:report/closed r))
              "Command applied to the available ancestor report after TTL flush"))
        (finally
          (teardown! ctx))))))

;; ---------------------------------------------------------------------------
;; Patch-series cover-letter broadcast
;; ---------------------------------------------------------------------------

(deftest cover-letter-broadcast
  (testing "Commands sent in reply to a patch-series cover letter
            propagate to every patch of the series, EXCEPT relation
            commands which stay on the cover only."
    (let [{:keys [conn] :as ctx} (setup-db!)]
      (try
        ;; A standalone bug to be used as the Related-to target.
        (store-and-process! conn
                            (mk-email {:mid "<other-bug@test.org>"
                                       :subject "[BUG] unrelated thing"
                                       :from "third@test.org"
                                       :date #inst "2026-06-01T08:00:00"
                                       :body "another report\n"})
                            "direct")
        ;; Cover letter (0/3) + 3 patches.
        (store-and-process! conn
                            (mk-email {:mid "<cov@test.org>"
                                       :subject "[PATCH cl 0/3] Refactor X"
                                       :from "user@test.org"
                                       :date #inst "2026-06-01T10:00:00"
                                       :body "Series intro.\n"})
                            "direct")
        (doseq [i [1 2 3]]
          (store-and-process! conn
                              (mk-email {:mid (str "<p" i "@test.org>")
                                         :subject (str "[PATCH cl " i "/3] step " i)
                                         :from "user@test.org"
                                         :date #inst "2026-06-01T10:01:00"
                                         :in-reply-to "<cov@test.org>"
                                         :body (str "step " i "\n\n"
                                                    "diff --git a/x.clj b/x.clj\n"
                                                    "--- a/x.clj\n+++ b/x.clj\n"
                                                    "@@ -1,1 +1,1 @@\n-a\n+b\n")})
                              "direct"))
        ;; Reply to the cover with a mix of broadcastable commands
        ;; and one relation command.
        (store-and-process! conn
                            (mk-email {:mid "<rev@test.org>"
                                       :subject "Re: [PATCH cl 0/3] Refactor X"
                                       :from "admin@test.org"
                                       :date #inst "2026-06-02T09:00:00"
                                       :in-reply-to "<cov@test.org>"
                                       :body (str "Acked.\n"
                                                  "Deadline: 30d\n"
                                                  "Related-to: <other-bug@test.org>\n")})
                            "direct")

        (let [db    (d/db conn)
              cover (get-report db "<cov@test.org>")
              p1    (get-report db "<p1@test.org>")
              p2    (get-report db "<p2@test.org>")
              p3    (get-report db "<p3@test.org>")
              related-to-other? (fn [r]
                                  (some #(= "<other-bug@test.org>"
                                            (:report/message-id %))
                                        (all-related r)))]
          (testing "Acked. propagates to cover and every patch"
            (is (some? (:report/acked cover)) "cover acked")
            (is (some? (:report/acked p1))    "patch 1/3 acked")
            (is (some? (:report/acked p2))    "patch 2/3 acked")
            (is (some? (:report/acked p3))    "patch 3/3 acked"))
          (testing "Deadline: propagates to cover and every patch"
            (is (some? (:report/deadline-value cover)) "cover has deadline")
            (is (some? (:report/deadline-value p1))    "patch 1/3 has deadline")
            (is (some? (:report/deadline-value p2))    "patch 2/3 has deadline")
            (is (some? (:report/deadline-value p3))    "patch 3/3 has deadline"))
          (testing "Related-to stays on the cover only"
            (is (related-to-other? cover)
                "cover is related-to the target bug")
            (is (not (related-to-other? p1))
                "patch 1/3 has no relation to the target")
            (is (not (related-to-other? p2))
                "patch 2/3 has no relation to the target")
            (is (not (related-to-other? p3))
                "patch 3/3 has no relation to the target")))
        (finally
          (teardown! ctx))))))

(deftest cover-letter-broadcast-closure
  (testing "Closed. on a cover letter closes every patch of the series."
    (let [{:keys [conn] :as ctx} (setup-db!)]
      (try
        (store-and-process! conn
                            (mk-email {:mid "<cov2@test.org>"
                                       :subject "[PATCH topic 0/2] series"
                                       :from "user@test.org"
                                       :date #inst "2026-06-03T10:00:00"
                                       :body "Series\n"})
                            "direct")
        (doseq [i [1 2]]
          (store-and-process! conn
                              (mk-email {:mid (str "<q" i "@test.org>")
                                         :subject (str "[PATCH topic " i "/2] step " i)
                                         :from "user@test.org"
                                         :date #inst "2026-06-03T10:01:00"
                                         :in-reply-to "<cov2@test.org>"
                                         :body (str "step " i "\n\n"
                                                    "diff --git a/y.clj b/y.clj\n"
                                                    "--- a/y.clj\n+++ b/y.clj\n"
                                                    "@@ -1,1 +1,1 @@\n-a\n+b\n")})
                              "direct"))
        (store-and-process! conn
                            (mk-email {:mid "<close-rev@test.org>"
                                       :subject "Re: [PATCH topic 0/2] series"
                                       :from "admin@test.org"
                                       :date #inst "2026-06-04T09:00:00"
                                       :in-reply-to "<cov2@test.org>"
                                       :body "Closed.\n"})
                            "direct")
        (let [db (d/db conn)
              cover (get-report db "<cov2@test.org>")
              q1    (get-report db "<q1@test.org>")
              q2    (get-report db "<q2@test.org>")]
          (is (some? (:report/closed cover)) "cover closed")
          (is (some? (:report/closed q1))    "patch 1/2 closed")
          (is (some? (:report/closed q2))    "patch 2/2 closed")
          (is (= :resolved (:report/close-reason cover)))
          (is (= :resolved (:report/close-reason q1)))
          (is (= :resolved (:report/close-reason q2))))
        (finally
          (teardown! ctx))))))

(deftest series-restart-keeps-own-cover-series
  (testing "A re-roll posted with a fresh cover letter supersedes the
            old series WITHOUT closing the series its own cover just
            created: the numbered patches join the cover's series, and
            a close command on the cover reaches them."
    (let [{:keys [conn] :as ctx} (setup-db!)]
      (try
        ;; v1 series without a cover: 1/3 root, 2/3 reply (aborted).
        (store-and-process! conn
                            (mk-email {:mid "<sr-v1-1@test.org>"
                                       :subject "[PATCH t 1/3] step a"
                                       :from "user@test.org"
                                       :date #inst "2026-06-10T10:00:00"
                                       :body (str "step a\n\n"
                                                  "diff --git a/z.clj b/z.clj\n"
                                                  "--- a/z.clj\n+++ b/z.clj\n"
                                                  "@@ -1,1 +1,1 @@\n-a\n+b\n")})
                            "direct")
        (store-and-process! conn
                            (mk-email {:mid "<sr-v1-2@test.org>"
                                       :subject "[PATCH t 2/3] step b"
                                       :from "user@test.org"
                                       :date #inst "2026-06-10T10:01:00"
                                       :in-reply-to "<sr-v1-1@test.org>"
                                       :body (str "step b\n\n"
                                                  "diff --git a/z.clj b/z.clj\n"
                                                  "--- a/z.clj\n+++ b/z.clj\n"
                                                  "@@ -1,1 +1,1 @@\n-b\n+c\n")})
                            "direct")
        ;; v2 re-roll: new thread with a cover, patches reply to it.
        (store-and-process! conn
                            (mk-email {:mid "<sr-cov@test.org>"
                                       :subject "[PATCH t v2 0/2] revamp"
                                       :from "user@test.org"
                                       :date #inst "2026-06-11T10:00:00"
                                       :body "Series v2\n"})
                            "direct")
        (doseq [i [1 2]]
          (store-and-process! conn
                              (mk-email {:mid (str "<sr-v2-" i "@test.org>")
                                         :subject (str "[PATCH t v2 " i "/2] step " i)
                                         :from "user@test.org"
                                         :date #inst "2026-06-11T10:01:00"
                                         :in-reply-to "<sr-cov@test.org>"
                                         :body (str "step " i "\n\n"
                                                    "diff --git a/z.clj b/z.clj\n"
                                                    "--- a/z.clj\n+++ b/z.clj\n"
                                                    "@@ -1,1 +1,1 @@\n-a\n+b\n")})
                              "direct"))
        (let [db (d/db conn)
              v2 (get-series-by-id db "t|user@test.org|2")]
          (testing "old v1 series is closed by the restart"
            (is (some? (:series/closed (get-series-by-id db "t|user@test.org|3")))))
          (testing "v2 patches join their own cover's series, still open"
            (is (nil? (:series/closed v2)))
            (is (= "<sr-cov@test.org>"
                   (get-in v2 [:series/cover-letter :email/message-id])))
            (is (= 2 (series-patch-count db "t|user@test.org|2")))
            (is (nil? (get-series-by-id db "t|user@test.org|2#2"))
                "no orphan series is created for the numbered patches")))
        ;; Applied. on the cover must now reach both patches.
        (store-and-process! conn
                            (mk-email {:mid "<sr-applied@test.org>"
                                       :subject "Re: [PATCH t v2 0/2] revamp"
                                       :from "admin@test.org"
                                       :date #inst "2026-06-12T09:00:00"
                                       :in-reply-to "<sr-cov@test.org>"
                                       :body "Applied. Thanks!\n"})
                            "direct")
        (let [db (d/db conn)]
          (is (some? (:report/closed (get-report db "<sr-v2-1@test.org>")))
              "patch 1/2 closed via cover broadcast")
          (is (some? (:report/closed (get-report db "<sr-v2-2@test.org>")))
              "patch 2/2 closed via cover broadcast"))
        (finally
          (teardown! ctx))))))

(deftest cover-letter-broadcast-supersede
  (testing "Superseded-by: on a cover letter supersedes every patch:
            each report closes with reason :superseded and points to
            the same target via :rel/supersedes."
    (let [{:keys [conn] :as ctx} (setup-db!)]
      (try
        ;; The Superseded-by: target must be a :patch (cross-type
        ;; supersede is rejected by the same-type check).
        (store-and-process! conn
                            (mk-email {:mid "<target-bug@test.org>"
                                       :subject "[PATCH replacement 1/1] the fix"
                                       :from "third@test.org"
                                       :date #inst "2026-06-05T08:00:00"
                                       :body (str "the fix\n\n"
                                                  "diff --git a/y.clj b/y.clj\n"
                                                  "--- a/y.clj\n+++ b/y.clj\n"
                                                  "@@ -1,1 +1,1 @@\n-a\n+b\n")})
                            "direct")
        (store-and-process! conn
                            (mk-email {:mid "<cov3@test.org>"
                                       :subject "[PATCH t3 0/2] old series"
                                       :from "user@test.org"
                                       :date #inst "2026-06-05T10:00:00"
                                       :body "Series\n"})
                            "direct")
        (doseq [i [1 2]]
          (store-and-process! conn
                              (mk-email {:mid (str "<r" i "@test.org>")
                                         :subject (str "[PATCH t3 " i "/2] step " i)
                                         :from "user@test.org"
                                         :date #inst "2026-06-05T10:01:00"
                                         :in-reply-to "<cov3@test.org>"
                                         :body (str "step " i "\n\n"
                                                    "diff --git a/z.clj b/z.clj\n"
                                                    "--- a/z.clj\n+++ b/z.clj\n"
                                                    "@@ -1,1 +1,1 @@\n-a\n+b\n")})
                              "direct"))
        (store-and-process! conn
                            (mk-email {:mid "<sup-rev@test.org>"
                                       :subject "Re: [PATCH t3 0/2] old series"
                                       :from "admin@test.org"
                                       :date #inst "2026-06-06T09:00:00"
                                       :in-reply-to "<cov3@test.org>"
                                       :body "Superseded-by: <target-bug@test.org>\n"})
                            "direct")
        (let [db    (d/db conn)
              cover (get-report db "<cov3@test.org>")
              r1    (get-report db "<r1@test.org>")
              r2    (get-report db "<r2@test.org>")
              target-mid (fn [r]
                           (some-> (superseded-by-target r) :report/message-id))]
          (doseq [[label r] [["cover" cover] ["patch 1/2" r1] ["patch 2/2" r2]]]
            (is (some? (:report/closed r))
                (str label " is closed"))
            (is (= :superseded (:report/close-reason r))
                (str label " has :superseded reason"))
            (is (= "<target-bug@test.org>" (target-mid r))
                (str label " is :rel/supersedes -> target-bug"))))
        (finally
          (teardown! ctx))))))

(deftest patch-close-stays-scoped-to-the-patch
  (testing "Applied. in reply to one patch closes only that patch: the
            series-wide close belongs to the cover letter alone, with
            or without a cover in the series."
    (let [{:keys [conn] :as ctx} (setup-db!)]
      (try
        ;; Series WITH a cover letter.
        (store-and-process! conn
                            (mk-email {:mid "<pcs-cov@test.org>"
                                       :subject "[PATCH pcs 0/2] with cover"
                                       :from "user@test.org"
                                       :date #inst "2026-07-01T10:00:00"
                                       :body "Cover letter.\n"})
                            "direct")
        (doseq [i [1 2]]
          (store-and-process! conn
                              (mk-email {:mid (str "<pcs-p" i "@test.org>")
                                         :subject (str "[PATCH pcs " i "/2] step " i)
                                         :from "user@test.org"
                                         :date #inst "2026-07-01T10:01:00"
                                         :in-reply-to "<pcs-cov@test.org>"
                                         :body (str "step " i "\n")})
                              "direct"))
        (store-and-process! conn
                            (mk-email {:mid "<pcs-applied@test.org>"
                                       :subject "Re: [PATCH pcs 1/2] step 1"
                                       :from "admin@test.org"
                                       :date #inst "2026-07-02T09:00:00"
                                       :in-reply-to "<pcs-p1@test.org>"
                                       :body "Applied.\n"})
                            "direct")
        (let [db (d/db conn)]
          (is (some? (:report/closed (get-report db "<pcs-p1@test.org>")))
              "the patch replied to is closed")
          (is (nil? (:report/closed (get-report db "<pcs-p2@test.org>")))
              "its sibling stays open")
          (is (nil? (:report/closed (get-report db "<pcs-cov@test.org>")))
              "the cover stays open"))
        ;; Series WITHOUT a cover letter.
        (doseq [i [1 2]]
          (store-and-process! conn
                              (mk-email {:mid (str "<pns-p" i "@test.org>")
                                         :subject (str "[PATCH pns " i "/2] step " i)
                                         :from "user@test.org"
                                         :date #inst "2026-07-03T10:00:00"
                                         :body (str "step " i "\n")})
                              "direct"))
        (store-and-process! conn
                            (mk-email {:mid "<pns-applied@test.org>"
                                       :subject "Re: [PATCH pns 1/2] step 1"
                                       :from "admin@test.org"
                                       :date #inst "2026-07-04T09:00:00"
                                       :in-reply-to "<pns-p1@test.org>"
                                       :body "Applied.\n"})
                            "direct")
        (let [db (d/db conn)]
          (is (some? (:report/closed (get-report db "<pns-p1@test.org>")))
              "the patch replied to is closed")
          (is (nil? (:report/closed (get-report db "<pns-p2@test.org>")))
              "its sibling stays open even without a cover letter"))
        (finally
          (teardown! ctx))))))

(deftest supersede-reaches-sibling-branch-revision
  (testing "v3 replying to a reviewer's comment (not to v2 directly) still
            closes v2 as :superseded.  Regression: v2 replied straight to
            v1, but the reviewer's comment -- and so v3 -- branched off v1
            on a different reply path that never mentions v2.  Walking only
            the immediate ancestor chain (`nearest-report-eids`) missed this
            still-open sibling; `open-patch-eids-by-sender` closes the gap."
    (let [{:keys [conn] :as ctx} (setup-db!)]
      (try
        (store-and-process! conn
                            (mk-email {:mid "<v1@test.org>"
                                       :subject "[PATCH] widget: fix the thing"
                                       :from "user@test.org"
                                       :date #inst "2026-06-08T19:15:00"
                                       :body "Initial patch.\n"})
                            "direct")
        (store-and-process! conn
                            (assoc (mk-email {:mid "<v2@test.org>"
                                              :subject "Re: [PATCH v2] widget: fix the thing"
                                              :from "user@test.org"
                                              :date #inst "2026-06-12T14:59:00"
                                              :in-reply-to "<v1@test.org>"
                                              :body "Updated version of the patch.\n"})
                                   :email/attachments [{:attachment/filename "0002-fix.patch"}])
                            "direct")
        ;; A reviewer's plain-discussion reply to v1 -- no [PATCH] label,
        ;; no attachment, so it never becomes a report of its own, but it
        ;; still threads (and its mid must be stored for the ancestor walk
        ;; to splice through it).
        (store-and-process! conn
                            (mk-email {:mid "<review@test.org>"
                                       :subject "Re: [PATCH] widget: fix the thing"
                                       :from "reviewer@test.org"
                                       :date #inst "2026-06-15T10:00:00"
                                       :in-reply-to "<v1@test.org>"
                                       :body "Some review comments, no patch attached.\n"})
                            "direct")
        ;; v3 replies to the reviewer's comment, not to v2.
        (store-and-process! conn
                            (assoc (mk-email {:mid "<v3@test.org>"
                                              :subject "Re: [PATCH v3] widget: fix the thing"
                                              :from "user@test.org"
                                              :date #inst "2026-06-17T06:37:00"
                                              :in-reply-to "<review@test.org>"
                                              :body "Third version, addressing review.\n"})
                                   :email/attachments [{:attachment/filename "0003-fix.patch"}])
                            "direct")
        (let [db (d/db conn)
              v1 (get-report db "<v1@test.org>")
              v2 (get-report db "<v2@test.org>")
              v3 (get-report db "<v3@test.org>")]
          (is (some? v3) "v3 report was created")
          (is (some? (:report/closed v1)) "v1 was closed (superseded by v2)")
          (is (some? (:report/closed v2))
              "v2 was closed (superseded by v3) despite not being on v3's reply chain")
          (is (= :superseded (:report/close-reason v2)))
          (is (nil? (:report/closed v3)) "v3 itself stays open"))
        (finally
          (teardown! ctx))))))

(deftest close-command-follows-supersession-chain
  (testing "Applied. in reply to a superseded revision's branch closes
            the live head of the supersession chain, even when that head
            lives on a sibling thread branch never mentioned by the reply."
    (let [{:keys [conn] :as ctx} (setup-db!)]
      (try
        (store-and-process! conn
                            (mk-email {:mid "<sc-v1@test.org>"
                                       :subject "[PATCH] table: speed up align"
                                       :from "user@test.org"
                                       :date #inst "2026-06-20T10:00:00"
                                       :body "Initial patch.\n"})
                            "direct")
        ;; v2 supersedes v1.
        (store-and-process! conn
                            (assoc (mk-email {:mid "<sc-v2@test.org>"
                                              :subject "Re: [PATCH v2] table: speed up align"
                                              :from "user@test.org"
                                              :date #inst "2026-06-21T10:00:00"
                                              :in-reply-to "<sc-v1@test.org>"
                                              :body "Updated patch.\n"})
                                   :email/attachments [{:attachment/filename "0002-align.patch"}])
                            "direct")
        ;; Review comment under v2: the branch the closer will reply to.
        (store-and-process! conn
                            (mk-email {:mid "<sc-review@test.org>"
                                       :subject "Re: [PATCH v2] table: speed up align"
                                       :from "reviewer@test.org"
                                       :date #inst "2026-06-22T10:00:00"
                                       :in-reply-to "<sc-v2@test.org>"
                                       :body "Looks good to me.\n"})
                            "direct")
        ;; v3 posted on its own branch supersedes v2 (sibling-branch
        ;; auto-supersession); the closer's branch never mentions it.
        (store-and-process! conn
                            (assoc (mk-email {:mid "<sc-v3@test.org>"
                                              :subject "Re: [PATCH v3] table: speed up align"
                                              :from "user@test.org"
                                              :date #inst "2026-06-23T10:00:00"
                                              :in-reply-to "<sc-v1@test.org>"
                                              :body "Third version.\n"})
                                   :email/attachments [{:attachment/filename "0003-align.patch"}])
                            "direct")
        ;; The maintainer replies on the review branch: nearest report
        ;; ancestor is v2, closed :superseded by then.
        (store-and-process! conn
                            (mk-email {:mid "<sc-applied@test.org>"
                                       :subject "Re: [PATCH v2] table: speed up align"
                                       :from "admin@test.org"
                                       :date #inst "2026-06-24T10:00:00"
                                       :in-reply-to "<sc-review@test.org>"
                                       :body "Applied, onto main.\n"})
                            "direct")
        (let [db (d/db conn)
              v1 (get-report db "<sc-v1@test.org>")
              v2 (get-report db "<sc-v2@test.org>")
              v3 (get-report db "<sc-v3@test.org>")]
          (is (= :superseded (:report/close-reason v1)) "v1 stays superseded")
          (is (= :superseded (:report/close-reason v2)) "v2 stays superseded")
          (is (some? (:report/closed v3))
              "the open head v3 is closed by the forwarded command")
          (is (= :resolved (:report/close-reason v3)))
          (is (= "admin@test.org" (:report/closed-address v3))))
        (finally
          (teardown! ctx))))))

(deftest supersede-does-not-cross-unrelated-threads
  (testing "Two patches from the same sender with the same (generic,
            recycled) subject, but that never share any common
            ancestor, must not be conflated: same sender is not enough
            -- `open-patch-eids-by-sender` candidates must also pass
            `shares-common-ancestor?`."
    (let [{:keys [conn] :as ctx} (setup-db!)]
      (try
        ;; An unrelated earlier patch, same sender, same recycled
        ;; subject text as the one below -- but a wholly separate topic.
        (store-and-process! conn
                            (mk-email {:mid "<typo-fix-1@test.org>"
                                       :subject "[PATCH] ox-html: fix typo in docstring"
                                       :from "user@test.org"
                                       :date #inst "2026-05-01T09:00:00"
                                       :body "Fix a typo in ox-html's docstring.\n"})
                            "direct")
        ;; A different, unrelated thread by the same sender -- e.g. they
        ;; hit "reply" on the wrong old email to start this new topic.
        (store-and-process! conn
                            (mk-email {:mid "<other-root@test.org>"
                                       :subject "[PATCH] ob-python: unrelated fix"
                                       :from "user@test.org"
                                       :date #inst "2026-05-10T09:00:00"
                                       :body "Some unrelated patch.\n"})
                            "direct")
        ;; "v2" of the *docstring* subject, but threaded under the
        ;; *unrelated* root above -- no ancestor in common with
        ;; <typo-fix-1@test.org>.
        (store-and-process! conn
                            (assoc (mk-email {:mid "<typo-fix-2@test.org>"
                                              :subject "Re: [PATCH v2] ox-html: fix typo in docstring"
                                              :from "user@test.org"
                                              :date #inst "2026-05-10T10:00:00"
                                              :in-reply-to "<other-root@test.org>"
                                              :body "A second, unrelated typo fix.\n"})
                                   :email/attachments [{:attachment/filename "0002-fix.patch"}])
                            "direct")
        (let [db     (d/db conn)
              typo1  (get-report db "<typo-fix-1@test.org>")
              other  (get-report db "<other-root@test.org>")
              typo2  (get-report db "<typo-fix-2@test.org>")]
          (is (some? typo2) "typo-fix-2 report was created")
          (is (nil? (:report/closed typo1))
              "typo-fix-1 stays open: same sender/subject but no common ancestor with typo-fix-2")
          (is (nil? (:report/closed other))
              "other-root stays open: different subject entirely"))
        (finally
          (teardown! ctx))))))

(deftest version-supersede-does-not-cross-series-siblings
  (testing "Two DIFFERENT patches in the same versioned series must not
            supersede each other just because they share a version and a
            colon-topic prefix (e.g. both '[PATCH vN k/m] ox-texinfo: ...'
            parse topic 'ox-texinfo').  Auto-supersession must key on the
            full normalized content subject, not the module prefix."
    (let [{:keys [conn] :as ctx} (setup-db!)]
      (try
        (store-and-process! conn
                            (mk-email {:mid "<cover@test.org>"
                                       :subject "[PATCH] ox-texinfo series"
                                       :from "user@test.org"
                                       :date #inst "2026-01-01T09:00:00"
                                       :body "Cover letter.\n"})
                            "direct")
        ;; Two distinct v1 patches sharing the colon-topic "ox-texinfo".
        (store-and-process! conn
                            (mk-email {:mid "<a1@test.org>"
                                       :subject "[PATCH v1 1/2] ox-texinfo: Add function for kbd macro"
                                       :from "user@test.org"
                                       :date #inst "2026-01-01T09:01:00"
                                       :in-reply-to "<cover@test.org>"
                                       :body "Patch A, v1.\n"})
                            "direct")
        (store-and-process! conn
                            (mk-email {:mid "<b1@test.org>"
                                       :subject "[PATCH v1 2/2] ox-texinfo: Define definition commands"
                                       :from "user@test.org"
                                       :date #inst "2026-01-01T09:02:00"
                                       :in-reply-to "<cover@test.org>"
                                       :body "Patch B, v1.\n"})
                            "direct")
        ;; Only A is resent as v2 -- B's v1 (different content, same topic
        ;; prefix and same version family) must survive untouched.
        (store-and-process! conn
                            (mk-email {:mid "<a2@test.org>"
                                       :subject "[PATCH v2 1/2] ox-texinfo: Add function for kbd macro"
                                       :from "user@test.org"
                                       :date #inst "2026-01-05T09:00:00"
                                       :in-reply-to "<cover@test.org>"
                                       :body "Patch A, v2.\n"})
                            "direct")
        (let [db (d/db conn)
              a1 (get-report db "<a1@test.org>")
              b1 (get-report db "<b1@test.org>")]
          (is (some? (:report/closed a1)) "A's v1 was superseded by A's v2 (same content)")
          (is (nil? (:report/closed b1))
              "B's v1 stays open: different content title, despite shared 'ox-texinfo' topic and matching version family"))
        ;; B is now resent as v2 too -- its v1 must close this time.
        (store-and-process! conn
                            (mk-email {:mid "<b2@test.org>"
                                       :subject "[PATCH v2 2/2] ox-texinfo: Define definition commands"
                                       :from "user@test.org"
                                       :date #inst "2026-01-06T09:00:00"
                                       :in-reply-to "<cover@test.org>"
                                       :body "Patch B, v2.\n"})
                            "direct")
        (let [db (d/db conn)
              b1 (get-report db "<b1@test.org>")]
          (is (some? (:report/closed b1)) "B's v1 was superseded by B's v2 (same content)"))
        (finally
          (teardown! ctx))))))

(defn- fp-attachment
  "A `git format-patch` attachment map with FILENAME and an internal
  Subject: line, so build-patch-entities stores :patch/filename +
  :patch/subject."
  [filename internal-subject]
  {:attachment/filename filename
   :attachment/data
   (str "From " (apply str (repeat 40 "0")) " Mon Sep 17 00:00:00 2001\n"
        "From: User <user@test.org>\n"
        "Subject: " internal-subject "\n"
        "\n"
        "diff --git a/x.el b/x.el\n--- a/x.el\n+++ b/x.el\n@@ -1 +1 @@\n-a\n+b\n")})

(deftest supersede-vetoed-when-patch-content-differs
  (testing "Two DIFFERENT patches from the same sender that share the
            email (thread) subject and a common ancestor must NOT
            supersede each other -- the attached patch's filename /
            internal Subject reveals they are distinct patches (real
            case: ob-screen 'respect custom screen location' vs 'support
            :var header parameter').  A genuine re-send of the SAME patch
            (matching filename) still supersedes."
    (let [{:keys [conn] :as ctx} (setup-db!)]
      (try
        ;; A: an open patch under a shared thread subject.
        (store-and-process! conn
                            (assoc (mk-email {:mid "<a@test.org>"
                                              :subject "[PATCH] ob-screen: babel improvements"
                                              :from "user@test.org"
                                              :date #inst "2023-03-19T00:00:00"
                                              :body "Patch A.\n"})
                                   :email/attachments
                                   [(fp-attachment "0001-ob-screen-respect-custom-screen-location.patch"
                                                   "[PATCH] ob-screen.el: respect custom screen location")])
                            "direct")
        ;; A reviewer reply (no patch) -- gives a cousin branch off A.
        (store-and-process! conn
                            (mk-email {:mid "<review@test.org>"
                                       :subject "Re: [PATCH] ob-screen: babel improvements"
                                       :from "reviewer@test.org"
                                       :date #inst "2023-03-20T00:00:00"
                                       :in-reply-to "<a@test.org>"
                                       :body "Looks interesting.\n"})
                            "direct")
        ;; B: a DIFFERENT patch, same thread subject, cousin branch.
        (store-and-process! conn
                            (assoc (mk-email {:mid "<b@test.org>"
                                              :subject "Re: [PATCH] ob-screen: babel improvements"
                                              :from "user@test.org"
                                              :date #inst "2025-07-21T00:00:00"
                                              :in-reply-to "<review@test.org>"
                                              :body "Patch B, unrelated change.\n"})
                                   :email/attachments
                                   [(fp-attachment "0001-ob-screen-support-var-header-parameter.patch"
                                                   "[PATCH] ob-screen.el: support :var header parameter")])
                            "direct")
        (let [db (d/db conn)
              a  (get-report db "<a@test.org>")]
          (is (nil? (:report/closed a))
              "A stays OPEN: B is a different patch (distinct filename/Subject) despite the shared thread subject"))
        ;; C: a genuine re-send of the SAME patch as A -- must supersede A.
        (store-and-process! conn
                            (assoc (mk-email {:mid "<c@test.org>"
                                              :subject "Re: [PATCH] ob-screen: babel improvements"
                                              :from "user@test.org"
                                              :date #inst "2025-08-01T00:00:00"
                                              :in-reply-to "<review@test.org>"
                                              :body "Patch A, resent.\n"})
                                   :email/attachments
                                   [(fp-attachment "0001-ob-screen-respect-custom-screen-location.patch"
                                                   "[PATCH] ob-screen.el: respect custom screen location")])
                            "direct")
        (let [db (d/db conn)
              a  (get-report db "<a@test.org>")]
          (is (some? (:report/closed a))
              "A is now closed: C carries the SAME patch (matching filename/Subject), a real re-send"))
        (finally
          (teardown! ctx))))))

(defn- fp-inline-body
  "An email body carrying an inline `git format-patch`, so
  build-patch-entities stores an \"inline.patch\" entry whose parsed
  :patch/subject is INTERNAL-SUBJECT."
  [internal-subject]
  (str "Hi,\n\nPatch below.\n\n"
       "From " (apply str (repeat 40 "0")) " Mon Sep 17 00:00:00 2001\n"
       "From: User <user@test.org>\n"
       "Subject: " internal-subject "\n"
       "\n"
       "diff --git a/x.el b/x.el\n--- a/x.el\n+++ b/x.el\n@@ -1 +1 @@\n-a\n+b\n"))

(deftest supersede-vetoed-for-inline-patches-too
  (testing "The content veto also protects inline-patch reports: the
            generic \"inline.patch\" filename carries no signal, but the
            parsed internal Subject does, so a reply attaching a
            DIFFERENT patch must not supersede an inline-patch original
            under the same thread subject -- while a re-send of the same
            patch (as attachment) still does."
    (let [{:keys [conn] :as ctx} (setup-db!)]
      (try
        ;; A: original submission, patch inline in the body only.
        (store-and-process! conn
                            (mk-email {:mid "<a@test.org>"
                                       :subject "[PATCH] tools: assorted improvements"
                                       :from "user@test.org"
                                       :date #inst "2026-01-05T00:00:00"
                                       :body (fp-inline-body "[PATCH] tools: add frobnicator")})
                            "direct")
        ;; B: DIFFERENT patch attached, same thread subject.
        (store-and-process! conn
                            (assoc (mk-email {:mid "<b@test.org>"
                                              :subject "Re: [PATCH] tools: assorted improvements"
                                              :from "user@test.org"
                                              :date #inst "2026-01-06T00:00:00"
                                              :in-reply-to "<a@test.org>"
                                              :body "Here is another one.\n"})
                                   :email/attachments
                                   [(fp-attachment "0001-tools-rewrite-defrobulator.patch"
                                                   "[PATCH] tools: rewrite defrobulator")])
                            "direct")
        (let [db (d/db conn)
              a  (get-report db "<a@test.org>")]
          (is (nil? (:report/closed a))
              "A stays OPEN: its inline internal Subject differs from B's patch"))
        ;; C: the SAME patch as A's inline one, re-sent as attachment.
        (store-and-process! conn
                            (assoc (mk-email {:mid "<c@test.org>"
                                              :subject "Re: [PATCH] tools: assorted improvements"
                                              :from "user@test.org"
                                              :date #inst "2026-01-07T00:00:00"
                                              :in-reply-to "<a@test.org>"
                                              :body "Rebased.\n"})
                                   :email/attachments
                                   [(fp-attachment "0001-tools-add-frobnicator.patch"
                                                   "[PATCH] tools: add frobnicator")])
                            "direct")
        (let [db (d/db conn)
              a  (get-report db "<a@test.org>")]
          (is (some? (:report/closed a))
              "A closed: C carries the same patch as A's inline one"))
        (finally
          (teardown! ctx))))))

(deftest supersede-sender-match-is-case-insensitive
  (testing "the sender-wide candidate search matches the author address
            case-insensitively: a sibling-branch v2 stored with a
            differently-cased From is still found and closed by v3"
    (let [{:keys [conn] :as ctx} (setup-db!)]
      (try
        (store-and-process! conn
                            (mk-email {:mid "<v1@test.org>"
                                       :subject "[PATCH] widget: fix the thing"
                                       :from "user@test.org"
                                       :date #inst "2026-06-08T19:15:00"
                                       :body "Initial patch.\n"})
                            "direct")
        ;; v2's From is cased differently than v1/v3's.
        (store-and-process! conn
                            (assoc (mk-email {:mid "<v2@test.org>"
                                              :subject "Re: [PATCH v2] widget: fix the thing"
                                              :from "User@Test.org"
                                              :date #inst "2026-06-12T14:59:00"
                                              :in-reply-to "<v1@test.org>"
                                              :body "Updated version of the patch.\n"})
                                   :email/attachments [{:attachment/filename "0002-fix.patch"}])
                            "direct")
        (store-and-process! conn
                            (mk-email {:mid "<review@test.org>"
                                       :subject "Re: [PATCH] widget: fix the thing"
                                       :from "reviewer@test.org"
                                       :date #inst "2026-06-15T10:00:00"
                                       :in-reply-to "<v1@test.org>"
                                       :body "Some review comments, no patch attached.\n"})
                            "direct")
        ;; v3 replies to the reviewer's comment: v2 is only reachable
        ;; through the sender-wide search, which must ignore the case
        ;; difference.
        (store-and-process! conn
                            (assoc (mk-email {:mid "<v3@test.org>"
                                              :subject "Re: [PATCH v3] widget: fix the thing"
                                              :from "user@test.org"
                                              :date #inst "2026-06-17T06:37:00"
                                              :in-reply-to "<review@test.org>"
                                              :body "Third version, addressing review.\n"})
                                   :email/attachments [{:attachment/filename "0003-fix.patch"}])
                            "direct")
        (let [db (d/db conn)
              v2 (get-report db "<v2@test.org>")]
          (is (some? (:report/closed v2))
              "v2 closed despite its differently-cased author address")
          (is (= :superseded (:report/close-reason v2))))
        (finally
          (teardown! ctx))))))

(deftest normalize-subject-widening
  (testing "parenthesized version markers and punctuation don't split
            otherwise-identical subjects (real corpus cases), but
            distinct wording still does"
    (let [norm #'digest/normalize-subject]
      (are [a b] (= (norm a) (norm b))
        "Re: [PATCH] (v3) New LaTeX code export option: engraved"
        "[PATCH] New LaTeX code export option: engraved"
        "[PATCH v1] org-agenda-clock-goto: Jump to closest entry and respect, filtering"
        "[PATCH v2] org-agenda-clock-goto: Jump to closest entry and respect filtering"
        "[PATCH] ox-md.el export code blocks using grave accents."
        "Re: [PATCH] ox-md.el export code blocks using grave accents"
        "Re: [PATCH] org-protocol: decode \"+\" in query part as space (v2)"
        "[PATCH] org-protocol: decode \"+\" in query part as space")
      (are [a b] (not= (norm a) (norm b))
        "[PATCH] Speed up tangling" "[PATCH] Improve tangling"
        ;; parenthesized text that is NOT a version marker is kept
        "add tests for ob-haskell (ghci)" "add tests for ob-haskell"
        ;; bare numbers are significant (series positions, counts)
        "[PATCH] Fix part 2" "[PATCH] Fix part 3")
      (is (nil? (norm "[PATCH]"))
          "a subject with nothing but tags normalizes to nil, never to a matchable empty string"))))

(deftest supersede-tolerates-version-marker-and-punctuation
  (testing "an unnumbered re-send whose subject differs only by a
            parenthesized version marker and punctuation still
            supersedes the original (real cases: '(v3) New LaTeX code
            export option' / 'respect, filtering')"
    (let [{:keys [conn] :as ctx} (setup-db!)]
      (try
        (store-and-process! conn
                            (mk-email {:mid "<v1@test.org>"
                                       :subject "[PATCH] widget: fix the thing, properly"
                                       :from "user@test.org"
                                       :date #inst "2026-06-08T19:15:00"
                                       :body "Initial patch.\n"})
                            "direct")
        (store-and-process! conn
                            (assoc (mk-email {:mid "<v2@test.org>"
                                              :subject "Re: [PATCH] (v2) widget: fix the thing properly"
                                              :from "user@test.org"
                                              :date #inst "2026-06-12T14:59:00"
                                              :in-reply-to "<v1@test.org>"
                                              :body "Updated version of the patch.\n"})
                                   :email/attachments [{:attachment/filename "0001-fix.patch"}])
                            "direct")
        (let [db (d/db conn)
              v1 (get-report db "<v1@test.org>")
              v2 (get-report db "<v2@test.org>")]
          (is (some? v2) "v2 report was created")
          (is (some? (:report/closed v1))
              "v1 closed despite the (v2) marker and comma in one subject")
          (is (= :superseded (:report/close-reason v1)))
          (is (nil? (:report/closed v2)) "v2 itself stays open"))
        (finally
          (teardown! ctx))))))

(deftest supersede-bare-patch-subject-falls-back-to-idents
  (testing "a bare '[PATCH]' subject normalizes to nil, so it can't
            subject-match anything (no more \"\" == \"\" collisions);
            instead, matching patch identifiers on both sides stand in
            for the subject (real case: 87fstreec8, tecosaur's
            proportional image widths patch resent as 'Re: [PATCH]')"
    (let [{:keys [conn] :as ctx} (setup-db!)]
      (try
        (store-and-process! conn
                            (assoc (mk-email {:mid "<a@test.org>"
                                              :subject "[PATCH]"
                                              :from "user@test.org"
                                              :date #inst "2021-05-01T00:00:00"
                                              :body "Patch A.\n"})
                                   :email/attachments
                                   [(fp-attachment "0001-org-Display-proportional-image-widths.patch"
                                                   "[PATCH] org: Display proportional image widths")])
                            "direct")
        ;; B: also subject-less, direct reply, but a DIFFERENT patch.
        (store-and-process! conn
                            (assoc (mk-email {:mid "<b@test.org>"
                                              :subject "Re: [PATCH]"
                                              :from "user@test.org"
                                              :date #inst "2021-05-02T00:00:00"
                                              :in-reply-to "<a@test.org>"
                                              :body "Patch B, unrelated.\n"})
                                   :email/attachments
                                   [(fp-attachment "0001-ox-html.el-remove-CDATA-strings.patch"
                                                   "[PATCH] ox-html.el: remove CDATA strings")])
                            "direct")
        (let [db (d/db conn)
              a  (get-report db "<a@test.org>")]
          (is (nil? (:report/closed a))
              "A stays OPEN: no subject signal and B's idents differ"))
        ;; C: subject-less re-send of the SAME patch -- idents stand in.
        (store-and-process! conn
                            (assoc (mk-email {:mid "<c@test.org>"
                                              :subject "Re: [PATCH]"
                                              :from "user@test.org"
                                              :date #inst "2021-05-03T00:00:00"
                                              :in-reply-to "<a@test.org>"
                                              :body "Patch A, updated.\n"})
                                   :email/attachments
                                   [(fp-attachment "0001-org-Display-proportional-image-widths.patch"
                                                   "[PATCH] org: Display proportional image widths")])
                            "direct")
        (let [db (d/db conn)
              a  (get-report db "<a@test.org>")]
          (is (some? (:report/closed a))
              "A closed: C carries the same patch, idents replace the missing subject")
          (is (= :superseded (:report/close-reason a))))
        (finally
          (teardown! ctx))))))

(deftest stale-old-version-does-not-close-newer
  (testing "A re-ingested old v2 (cross-post, resend, out-of-order
            ingestion) must not close the live v3: the version-order
            guard spares candidates newer than the new patch."
    (let [{:keys [conn] :as ctx} (setup-db!)]
      (try
        (store-and-process! conn
                            (mk-email {:mid "<sv-v1@test.org>"
                                       :subject "[PATCH] widget: fix the frobnicator"
                                       :from "user@test.org"
                                       :date #inst "2026-06-01T10:00:00"
                                       :body "Initial patch.\n"})
                            "direct")
        (store-and-process! conn
                            (assoc (mk-email {:mid "<sv-v2@test.org>"
                                              :subject "Re: [PATCH v2] widget: fix the frobnicator"
                                              :from "user@test.org"
                                              :date #inst "2026-06-02T10:00:00"
                                              :in-reply-to "<sv-v1@test.org>"
                                              :body "Second version.\n"})
                                   :email/attachments [{:attachment/filename "0002-fix.patch"}])
                            "direct")
        (store-and-process! conn
                            (assoc (mk-email {:mid "<sv-v3@test.org>"
                                              :subject "Re: [PATCH v3] widget: fix the frobnicator"
                                              :from "user@test.org"
                                              :date #inst "2026-06-03T10:00:00"
                                              :in-reply-to "<sv-v2@test.org>"
                                              :body "Third version.\n"})
                                   :email/attachments [{:attachment/filename "0003-fix.patch"}])
                            "direct")
        ;; A copy of v2 arrives late (new mid, same subject and version),
        ;; threaded under v1 -- a cross-post or resend delivered out of
        ;; order.
        (store-and-process! conn
                            (assoc (mk-email {:mid "<sv-v2-copy@test.org>"
                                              :subject "Re: [PATCH v2] widget: fix the frobnicator"
                                              :from "user@test.org"
                                              :date #inst "2026-06-02T11:00:00"
                                              :in-reply-to "<sv-v1@test.org>"
                                              :body "Second version (copy).\n"})
                                   :email/attachments [{:attachment/filename "0002-fix.patch"}])
                            "direct")
        (let [db (d/db conn)
              v2 (get-report db "<sv-v2@test.org>")
              v3 (get-report db "<sv-v3@test.org>")]
          (is (some? (get-report db "<sv-v2-copy@test.org>"))
              "the re-ingested v2 copy still creates its own report")
          (is (= :superseded (:report/close-reason v2)) "v2 stays superseded by v3")
          (is (nil? (:report/closed v3))
              "the live v3 must NOT be closed by the re-ingested old v2"))
        (finally
          (teardown! ctx))))))

(deftest supersede-reaches-sender-patches-without-nearest-report
  (testing "v1 posted in reply to a non-report mail is still superseded
            by a v2 posted on another branch of the same thread, even
            though no thread ancestor of v2 is a report (`nearest-eids`
            is empty): the sender-wide candidate pool must not be gated
            on nearest thread reports."
    (let [{:keys [conn] :as ctx} (setup-db!)]
      (try
        ;; A plain discussion mail: stored, but no report.
        (store-and-process! conn
                            (mk-email {:mid "<nn-root@test.org>"
                                       :subject "Some discussion"
                                       :from "someone@test.org"
                                       :date #inst "2026-07-01T10:00:00"
                                       :body "Just talking.\n"})
                            "direct")
        ;; v1 posted as a reply to the discussion.
        (store-and-process! conn
                            (mk-email {:mid "<nn-v1@test.org>"
                                       :subject "[PATCH] widget: fix frob"
                                       :from "user@test.org"
                                       :date #inst "2026-07-02T10:00:00"
                                       :in-reply-to "<nn-root@test.org>"
                                       :body "Initial patch.\n"})
                            "direct")
        ;; Another non-report reply on a sibling branch.
        (store-and-process! conn
                            (mk-email {:mid "<nn-branch@test.org>"
                                       :subject "Re: Some discussion"
                                       :from "reviewer@test.org"
                                       :date #inst "2026-07-03T10:00:00"
                                       :in-reply-to "<nn-root@test.org>"
                                       :body "Interesting.\n"})
                            "direct")
        ;; v2 on that sibling branch: none of its thread ancestors is a
        ;; report, but v1 shares the thread root with it.
        (store-and-process! conn
                            (assoc (mk-email {:mid "<nn-v2@test.org>"
                                              :subject "Re: [PATCH v2] widget: fix frob"
                                              :from "user@test.org"
                                              :date #inst "2026-07-04T10:00:00"
                                              :in-reply-to "<nn-branch@test.org>"
                                              :body "Second version.\n"})
                                   :email/attachments [{:attachment/filename "0002-frob.patch"}])
                            "direct")
        (let [db (d/db conn)
              v1 (get-report db "<nn-v1@test.org>")
              v2 (get-report db "<nn-v2@test.org>")]
          (is (some? v2) "v2 report was created")
          (is (some? (:report/closed v1))
              "v1 closed despite no report among v2's thread ancestors")
          (is (= :superseded (:report/close-reason v1)))
          (is (nil? (:report/closed v2)) "v2 stays open"))
        (finally
          (teardown! ctx))))))

(deftest topic-less-series-do-not-conflate
  (testing "Two topic-less series (no bracket topic, no colon topic)
            from the same sender on unrelated threads stay distinct,
            and the second cover letter must not overwrite the first
            series' cover: without a topic there is no safe fallback
            key, so threading is the only grouping mechanism."
    (let [{:keys [conn] :as ctx} (setup-db!)]
      (try
        ;; March thread: cover + one patch.
        (store-and-process! conn
                            (mk-email {:mid "<tl-cover-a@test.org>"
                                       :subject "[PATCH 0/2] Adding more tests"
                                       :from "user@test.org"
                                       :date #inst "2026-03-01T10:00:00"
                                       :body "Cover letter A.\n"})
                            "direct")
        (store-and-process! conn
                            (mk-email {:mid "<tl-a1@test.org>"
                                       :subject "[PATCH 1/2] Add tests"
                                       :from "user@test.org"
                                       :date #inst "2026-03-01T10:01:00"
                                       :in-reply-to "<tl-cover-a@test.org>"
                                       :body "Patch A1.\n"})
                            "direct")
        ;; June thread: unrelated cover + patch, same sender, same total.
        (store-and-process! conn
                            (mk-email {:mid "<tl-cover-b@test.org>"
                                       :subject "[PATCH 0/2] Removing old cruft"
                                       :from "user@test.org"
                                       :date #inst "2026-06-01T10:00:00"
                                       :body "Cover letter B.\n"})
                            "direct")
        (store-and-process! conn
                            (mk-email {:mid "<tl-b1@test.org>"
                                       :subject "[PATCH 1/2] Remove cruft"
                                       :from "user@test.org"
                                       :date #inst "2026-06-01T10:01:00"
                                       :in-reply-to "<tl-cover-b@test.org>"
                                       :body "Patch B1.\n"})
                            "direct")
        (let [db (d/db conn)
              a1 (get-report db "<tl-a1@test.org>")
              b1 (get-report db "<tl-b1@test.org>")
              series-a (:report/series a1)
              series-b (:report/series b1)]
          (is (some? series-a) "A1 joined a series")
          (is (some? series-b) "B1 joined a series")
          (is (not= (:series/id series-a) (:series/id series-b))
              "the two topic-less threads must not share a series")
          (is (= "<tl-cover-a@test.org>"
                 (get-in series-a [:series/cover-letter :email/message-id]))
              "series A keeps its own cover letter")
          (is (= "<tl-cover-b@test.org>"
                 (get-in series-b [:series/cover-letter :email/message-id]))
              "series B's cover letter did not overwrite series A's"))
        (finally
          (teardown! ctx))))))

;; ---------------------------------------------------------------------------
;; report-entity :report/has-ics is scoped to announcements
;; ---------------------------------------------------------------------------

(deftest report-entity-has-ics-scoped-to-announcements
  (let [email {:email/attachments [{:attachment/filename     "invite.ics"
                                    :attachment/content-type "text/calendar"}]
               :email/author-address "alice@test.org"}
        has-ics (fn [rtype]
                  (:report/has-ics
                   (digest/report-entity 1 "<m@test.org>" {:type rtype} nil email nil)))]
    (testing "announcement carrying an .ics is flagged"
      (is (true? (has-ics :announcement))))
    (testing "other types are never flagged, even with an .ics attachment"
      (is (false? (has-ics :bug)))
      (is (false? (has-ics :patch))))))

;; ---------------------------------------------------------------------------
;; report-entity stores :report/patches at creation
;; ---------------------------------------------------------------------------

(def ^:private format-patch-text
  (str "From 0123456789abcdef0123456789abcdef01234567 Mon Sep 17 00:00:00 2001\n"
       "From: Alice <alice@test.org>\n"
       "Date: Thu, 1 May 2026 10:00:00 +0000\n"
       "Subject: [PATCH] Fix the parser\n"
       "\n"
       "---\n"
       "diff --git a/parser.el b/parser.el\n"))

(deftest report-entity-stores-patches-at-creation
  (testing "an email with a .patch attachment yields :report/patches"
    (let [email {:email/attachments [{:attachment/filename "0001-fix.patch"
                                      :attachment/content-type "text/x-diff"
                                      :attachment/data format-patch-text}]
                 :email/author-address "alice@test.org"}
          entity (digest/report-entity 1 "<m@test.org>" {:type :patch} nil email nil)
          patches (:report/patches entity)]
      (is (= 1 (count patches)))
      (is (= "0001-fix.patch" (:patch/filename (first patches))))
      (is (= :attachment (:patch/source (first patches))))))
  (testing "an email without patch content has no :report/patches key"
    (let [entity (digest/report-entity 1 "<m@test.org>" {:type :bug} nil
                                       {:email/author-address "alice@test.org"
                                        :email/body-text "it crashes\n"}
                                       nil)]
      (is (not (contains? entity :report/patches))))))

;; ---------------------------------------------------------------------------
;; Pending emails surface their patches at creation, without duplication
;; on the TTL-flush retry (regression: patches used to wait on Phase 4,
;; which pending emails skip)
;; ---------------------------------------------------------------------------

(deftest pending-patch-stored-at-creation
  (testing "A [PATCH] reply whose parent is missing gets its patches immediately."
    (let [{:keys [conn] :as ctx} (setup-db!)]
      (try
        (store-and-process!
         conn
         (assoc (mk-email {:mid "<pend-patch@test.org>"
                           :subject "[PATCH] Fix the parser"
                           :from "user@test.org"
                           :date #inst "2026-05-01T10:00:00"
                           :in-reply-to "<never-ingested@test.org>"
                           :body "Here is the fix.\n"})
                :email/attachments [{:attachment/filename "0001-fix.patch"
                                     :attachment/content-type "text/x-diff"
                                     :attachment/data format-patch-text}])
         "direct")
        (let [db (d/db conn)
              r  (get-report db "<pend-patch@test.org>")]
          (is (true? (pending? db "<pend-patch@test.org>"))
              "Reply should be pending: its parent was never ingested")
          (is (= ["0001-fix.patch"] (mapv :patch/filename (:report/patches r)))
              "Patches should be stored at creation despite the pending flag"))

        ;; TTL-flush retries the pending email; the :store-patches hook
        ;; must not add duplicate patch components.
        (digest/flush-stale-pending! conn source-map sources 0)
        (let [db (d/db conn)
              r  (get-report db "<pend-patch@test.org>")]
          (is (false? (pending? db "<pend-patch@test.org>"))
              "Flush should clear the pending flag")
          (is (= ["0001-fix.patch"] (mapv :patch/filename (:report/patches r)))
              "Flush retry must not duplicate the stored patches"))
        (finally
          (teardown! ctx))))))

(deftest trailer-collection
  (testing "Git person trailers in replies are collected on patch reports."
    (let [{:keys [conn] :as ctx} (setup-db!)]
      (try
        ;; A standalone bug: trailers must never land on non-patch reports.
        (store-and-process! conn
                            (mk-email {:mid "<tr-bug@test.org>"
                                       :subject "[BUG] something"
                                       :from "user@test.org"
                                       :date #inst "2026-06-01T09:00:00"
                                       :body "it is broken\n"})
                            "direct")
        ;; Cover letter (0/2) + 2 patches.  The patch emails carry their
        ;; own Signed-off-by, which must NOT leak onto the cover.
        (store-and-process! conn
                            (mk-email {:mid "<tr-cov@test.org>"
                                       :subject "[PATCH tr 0/2] Refactor Y"
                                       :from "user@test.org"
                                       :date #inst "2026-06-01T10:00:00"
                                       :body "Series intro.\n"})
                            "direct")
        (doseq [i [1 2]]
          (store-and-process! conn
                              (mk-email {:mid (str "<tr-p" i "@test.org>")
                                         :subject (str "[PATCH tr " i "/2] step " i)
                                         :from "user@test.org"
                                         :date #inst "2026-06-01T10:01:00"
                                         :in-reply-to "<tr-cov@test.org>"
                                         :body (str "step " i "\n\n"
                                                    "Signed-off-by: User <user@test.org>\n"
                                                    "diff --git a/y.clj b/y.clj\n"
                                                    "--- a/y.clj\n+++ b/y.clj\n"
                                                    "@@ -1,1 +1,1 @@\n-a\n+b\n")})
                              "direct"))
        ;; Review of patch 1/2 only: trailer stays on that patch.
        (store-and-process! conn
                            (mk-email {:mid "<tr-rev1@test.org>"
                                       :subject "Re: [PATCH tr 1/2] step 1"
                                       :from "rev@test.org"
                                       :date #inst "2026-06-02T09:00:00"
                                       :in-reply-to "<tr-p1@test.org>"
                                       :body (str "Nice.\n\n"
                                                  "> Signed-off-by: User <user@test.org>\n\n"
                                                  "Reviewed-by: Rev One <rev@test.org>\n")})
                            "direct")
        ;; Ack on the cover: broadcast to every patch of the series.
        (store-and-process! conn
                            (mk-email {:mid "<tr-rev2@test.org>"
                                       :subject "Re: [PATCH tr 0/2] Refactor Y"
                                       :from "admin@test.org"
                                       :date #inst "2026-06-02T10:00:00"
                                       :in-reply-to "<tr-cov@test.org>"
                                       :body "Acked-by: Admin <admin@test.org>\n"})
                            "direct")
        ;; Trailer on the bug report: ignored (non-patch).
        (store-and-process! conn
                            (mk-email {:mid "<tr-rev3@test.org>"
                                       :subject "Re: [BUG] something"
                                       :from "rev@test.org"
                                       :date #inst "2026-06-02T11:00:00"
                                       :in-reply-to "<tr-bug@test.org>"
                                       :body "Reviewed-by: Rev One <rev@test.org>\n"})
                            "direct")

        (let [db    (d/db conn)
              bug   (get-report db "<tr-bug@test.org>")
              cover (get-report db "<tr-cov@test.org>")
              p1    (get-report db "<tr-p1@test.org>")
              p2    (get-report db "<tr-p2@test.org>")
              trailers #(set (:report/trailers %))]
          (testing "a review of one patch stays on that patch"
            (is (contains? (trailers p1) "Reviewed-by: Rev One <rev@test.org>"))
            (is (not (contains? (trailers p2) "Reviewed-by: Rev One <rev@test.org>"))))
          (testing "a trailer on the cover broadcasts to the series"
            (is (contains? (trailers cover) "Acked-by: Admin <admin@test.org>"))
            (is (contains? (trailers p1) "Acked-by: Admin <admin@test.org>"))
            (is (contains? (trailers p2) "Acked-by: Admin <admin@test.org>")))
          (testing "patch emails do not leak their Signed-off-by onto parents"
            (is (not-any? #(str/starts-with? % "Signed-off-by:") (trailers cover))))
          (testing "quoted trailers in reviews are not collected"
            (is (not-any? #(str/starts-with? % "Signed-off-by:") (trailers p1))))
          (testing "non-patch reports collect nothing"
            (is (empty? (trailers bug)))))
        (finally
          (teardown! ctx))))))

(deftest trailer-collection-edges
  (testing "Standalone patch (no series) and pending-reply rescue."
    (let [{:keys [conn] :as ctx} (setup-db!)]
      (try
        ;; The review arrives BEFORE its patch: flagged pending, then
        ;; rescued by retry-pending-in-shared-thread! when the patch
        ;; shows up.
        (store-and-process! conn
                            (mk-email {:mid "<te-rev@test.org>"
                                       :subject "Re: [PATCH] solo fix"
                                       :from "rev@test.org"
                                       :date #inst "2026-06-02T09:00:00"
                                       :in-reply-to "<te-p@test.org>"
                                       :body "Reviewed-by: Rev One <rev@test.org>\n"})
                            "direct")
        (is (pending? (d/db conn) "<te-rev@test.org>")
            "review of an unknown patch is deferred")
        (store-and-process! conn
                            (mk-email {:mid "<te-p@test.org>"
                                       :subject "[PATCH] solo fix"
                                       :from "user@test.org"
                                       :date #inst "2026-06-01T10:00:00"
                                       :body (str "fix\n\n"
                                                  "diff --git a/x b/x\n"
                                                  "--- a/x\n+++ b/x\n"
                                                  "@@ -1,1 +1,1 @@\n-a\n+b\n")})
                            "direct")
        (is (= #{"Reviewed-by: Rev One <rev@test.org>"}
               (set (:report/trailers (get-report (d/db conn) "<te-p@test.org>"))))
            "rescued review lands on the standalone patch (no cover to broadcast)")
        (finally
          (teardown! ctx))))))

;; ---------------------------------------------------------------------------
;; Applying a patch supersedes its stale upthread revisions
;; ---------------------------------------------------------------------------

(deftest applied-revision-supersedes-upthread-patches
  (testing "Closing a patch as :resolved supersedes the still-open patch
            reports upthread -- the cross-sender revision case that
            arrival-time auto-supersession (same-sender) misses."
    (let [{:keys [conn] :as ctx} (setup-db!)]
      (try
        (store-and-process! conn
                            (mk-email {:mid "<ar-v1@test.org>"
                                       :subject "[PATCH] widget: fix the thing"
                                       :from "user@test.org"
                                       :date #inst "2026-07-01T10:00:00"
                                       :body "Initial patch.\n"})
                            "direct")
        ;; The maintainer posts an improved version in the same thread:
        ;; different sender and base subject, so arrival-time
        ;; supersession stays out of the picture.
        (store-and-process! conn
                            (assoc (mk-email {:mid "<ar-v2@test.org>"
                                              :subject "Re: [PATCH] widget: improved fix"
                                              :from "maint@test.org"
                                              :date #inst "2026-07-02T10:00:00"
                                              :in-reply-to "<ar-v1@test.org>"
                                              :body "Improved version attached.\n"})
                                   :email/attachments
                                   [{:attachment/filename "0001-improved.patch"}])
                            "direct")
        (is (nil? (:report/closed (get-report (d/db conn) "<ar-v1@test.org>")))
            "v1 still open after the cross-sender v2 arrives")
        (store-and-process! conn
                            (mk-email {:mid "<ar-applied@test.org>"
                                       :subject "Re: [PATCH] widget: improved fix"
                                       :from "admin@test.org"
                                       :date #inst "2026-07-03T09:00:00"
                                       :in-reply-to "<ar-v2@test.org>"
                                       :refs ["<ar-v1@test.org>" "<ar-v2@test.org>"]
                                       :body "Applied.\n"})
                            "direct")
        (let [db (d/db conn)
              v1 (get-report db "<ar-v1@test.org>")
              v2 (get-report db "<ar-v2@test.org>")]
          (is (some? (:report/closed v2)) "applied v2 is closed")
          (is (= :resolved (:report/close-reason v2)))
          (is (some? (:report/closed v1))
              "stale v1 closed by the application of v2")
          (is (= :superseded (:report/close-reason v1)))
          (is (= "<ar-v2@test.org>"
                 (some-> (superseded-by-target v1) :report/message-id))
              "v1 records being superseded by v2"))
        (finally
          (teardown! ctx))))))

(deftest applied-revision-respects-patch-triggers-opt-out
  (testing "Sources with :patch-triggers? false skip the upthread
            supersession, like every other :resolved propagation."
    (let [{:keys [conn] :as ctx} (setup-db!)]
      (try
        (store-and-process! conn
                            (mk-email {:mid "<nt-v1@test.org>"
                                       :subject "[PATCH] gadget: fix"
                                       :from "user@test.org"
                                       :date #inst "2026-07-01T10:00:00"
                                       :body "Initial patch.\n"})
                            "no-triggers")
        (store-and-process! conn
                            (assoc (mk-email {:mid "<nt-v2@test.org>"
                                              :subject "Re: [PATCH] gadget: better fix"
                                              :from "maint@test.org"
                                              :date #inst "2026-07-02T10:00:00"
                                              :in-reply-to "<nt-v1@test.org>"
                                              :body "Better version attached.\n"})
                                   :email/attachments
                                   [{:attachment/filename "0001-better.patch"}])
                            "no-triggers")
        (store-and-process! conn
                            (mk-email {:mid "<nt-applied@test.org>"
                                       :subject "Re: [PATCH] gadget: better fix"
                                       :from "admin@test.org"
                                       :date #inst "2026-07-03T09:00:00"
                                       :in-reply-to "<nt-v2@test.org>"
                                       :refs ["<nt-v1@test.org>" "<nt-v2@test.org>"]
                                       :body "Applied.\n"})
                            "no-triggers")
        (let [db (d/db conn)]
          (is (some? (:report/closed (get-report db "<nt-v2@test.org>"))))
          (is (nil? (:report/closed (get-report db "<nt-v1@test.org>")))
              "opt-out source: v1 stays open"))
        (finally
          (teardown! ctx))))))

;; ---------------------------------------------------------------------------
;; thread-lookup arbitration (pure)
;; ---------------------------------------------------------------------------

(deftest thread-step-arbitration
  (let [s0 {:all #{} :nearest nil :cand nil}]
    (testing "a root at the closest mid decides alone (patch shadows cover)"
      (is (= {:all #{1 2} :nearest #{1} :cand nil}
             (digest/thread-step s0 {:root 1 :encl #{2}}))))
    (testing "an enclosing-only hit stays undecided"
      (is (= {:all #{2 3} :nearest nil :cand #{2 3}}
             (digest/thread-step s0 {:root nil :encl #{2 3}}))))
    (testing "a later root inside the candidates is the closest container"
      (is (= #{2} (:nearest (digest/thread-step
                             {:all #{2 3} :nearest nil :cand #{2 3}}
                             {:root 2 :encl #{}})))))
    (testing "a later root outside the candidates settles on them"
      (is (= #{2 3} (:nearest (digest/thread-step
                               {:all #{2 3} :nearest nil :cand #{2 3}}
                               {:root 9 :encl #{}})))))
    (testing "a decided :nearest never moves, :all keeps growing"
      (is (= {:all #{1 7 9} :nearest #{1} :cand nil}
             (digest/thread-step {:all #{1} :nearest #{1} :cand nil}
                                 {:root 9 :encl #{7}}))))
    (testing "a mid with no report changes nothing"
      (is (= s0 (digest/thread-step s0 {:root nil :encl #{}}))))
    (testing "finalize settles an undecided walk on the candidates"
      (is (= {:all #{2 3} :nearest #{2 3}}
             (digest/finalize-thread-lookup
              {:all #{2 3} :nearest nil :cand #{2 3}}))))
    (testing "finalize with nothing found leaves :nearest nil"
      (is (= {:all #{} :nearest nil}
             (digest/finalize-thread-lookup s0))))))

;; ---------------------------------------------------------------------------
;; Failure recording: no duplicate, no noise
;; ---------------------------------------------------------------------------

(deftest closed-report-bareword-records-no-scope-failure
  (testing "A bareword that cannot apply to a closed report is not scope-checked."
    (let [{:keys [conn] :as ctx} (setup-db!)
          recorded (atom [])
          smap     (assoc-in source-map ["direct" :commands]
                             {:closed {:scope :maintainer}})
          process! (fn [m]
                     (d/transact! conn [(assoc m :email/source "direct")])
                     (let [eid (lookup/email-eid (d/db conn) (:email/message-id m))]
                       (digest/process-email!
                        conn smap sources
                        (d/pull (d/db conn) digest/email-pull-pattern eid))))]
      (try
        (with-redefs [commands/record-failure! (fn [entry] (swap! recorded conj entry))]
          (process! (mk-email {:mid "<cl-bug@test.org>"
                               :subject "[BUG] crash"
                               :from "user@test.org"
                               :date #inst "2026-05-01T10:00:00"
                               :body "broken\n"}))
          (process! (mk-email {:mid "<cl-close@test.org>"
                               :subject "Re: [BUG] crash"
                               :from "admin@test.org"
                               :date #inst "2026-05-01T11:00:00"
                               :in-reply-to "<cl-bug@test.org>"
                               :body "Closed.\n"}))
          (is (some? (:report/closed (get-report (d/db conn) "<cl-bug@test.org>"))))
          (is (empty? @recorded))
          (process! (mk-email {:mid "<cl-late@test.org>"
                               :subject "Re: [BUG] crash"
                               :from "user@test.org"
                               :date #inst "2026-05-01T12:00:00"
                               :in-reply-to "<cl-bug@test.org>"
                               :body "Closed.\n"}))
          (is (empty? @recorded)
              "no :insufficient-scope failure for a no-op word on a closed report"))
        (finally
          (teardown! ctx))))))
