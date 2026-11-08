(ns bone.main-test
  (:require [clojure.test :refer [deftest is testing]]
            [bone.common :as common]
            [bone.digest :as digest]
            [bone.test-helpers :as th]
            [datalevin.core :as d]
            [bone.main :as main])
  (:import [java.util Date]))

(def parse-fetch (var-get #'main/parse-fetch))
(def cli-fetch->map (var-get #'main/cli-fetch->map))
(def run-opts (var-get #'main/run-opts))
(def load-context (var-get #'main/load-context))
(def parse-main-args (var-get #'main/parse-main-args))
(def max-contiguous-safe-uid (var-get #'main/max-contiguous-safe-uid))
(def connection-loss? (var-get #'main/connection-loss?))

;; ---------------------------------------------------------------------------
;; :fetch option
;; ---------------------------------------------------------------------------

(deftest parse-fetch-test
  (testing "{:limit N}: a positive integer, alone"
    (is (= {:limit 50} (parse-fetch {:limit 50})))
    (is (= {:limit 1}  (parse-fetch {:limit 1})))
    (is (thrown? Exception (parse-fetch {:limit 0})))
    (is (thrown? Exception (parse-fetch {:limit -5})))
    (is (thrown? Exception (parse-fetch {:limit "50"})))
    (is (thrown? Exception (parse-fetch {:limit 50 :since "30d"})))
    (is (thrown? Exception (parse-fetch {:limit 50 :start "2020-01-01"}))))
  (testing "{:since \"Nd\"}: a duration in any unit, alone, never an ISO date or a number"
    (is (instance? Date (:since (parse-fetch {:since "30d"}))))
    (doseq [u ["5d" "3w" "2m" "1y"]]
      (is (some? (:since (parse-fetch {:since u}))) u))
    (is (thrown? Exception (parse-fetch {:since "2020-01-01"})))
    (is (thrown? Exception (parse-fetch {:since "50"})))
    (is (thrown? Exception (parse-fetch {:since 50})))
    (is (thrown? Exception (parse-fetch {:since "30d" :end "2020-01-01"}))))
  (testing "{:start ISO} and {:end ISO} translate to mailseq :since / :before"
    (let [{:keys [since before]} (parse-fetch {:start "2020-01-01"})]
      (is (instance? Date since))
      (is (nil? before)))
    (let [{:keys [since before]} (parse-fetch {:end "2022-01-01"})]
      (is (nil? since))
      (is (instance? Date before)))
    (let [{:keys [since before]} (parse-fetch {:start "2020-01-01" :end "2022-01-01"})]
      (is (instance? Date since))
      (is (instance? Date before))
      (is (.before since before)))
    (is (thrown? Exception (parse-fetch {:start "30d"})))
    (is (thrown? Exception (parse-fetch {:end "30d"})))
    (is (thrown? Exception (parse-fetch {:start "2022-01-01" :end "2020-01-01"}))
        ":start must be strictly before :end")
    (is (thrown? Exception (parse-fetch {:start "2020-01-01" :end "2020-01-01"})))
    (is (thrown? Exception (parse-fetch {:start "2020-01-01" :foo 1}))
        "extraneous keys rejected"))
  (testing "anything but a non-empty map is rejected"
    (is (thrown? Exception (parse-fetch 50)))
    (is (thrown? Exception (parse-fetch "30d")))
    (is (thrown? Exception (parse-fetch "2020-01-01")))
    (is (thrown? Exception (parse-fetch nil)))
    (is (thrown? Exception (parse-fetch [:limit 50])))
    (is (thrown? Exception (parse-fetch {})))))

(deftest cli-fetch->map-lifts
  (testing "bare integer => :limit"
    (is (= {:limit 50} (cli-fetch->map "50"))))
  (testing "duration => :since"
    (is (= {:since "30d"} (cli-fetch->map "30d")))
    (is (= {:since "6w"}  (cli-fetch->map "6w"))))
  (testing "ISO date => :start"
    (is (= {:start "2020-01-01"} (cli-fetch->map "2020-01-01"))))
  (testing "invalid rejected"
    (is (thrown? Exception (cli-fetch->map "foo")))
    (is (thrown? Exception (cli-fetch->map "30 days")))))

;; ---------------------------------------------------------------------------
;; CLI argument parsing
;; ---------------------------------------------------------------------------

(deftest parse-main-args-nominal
  (testing "no args: batch mode, no CLI fetch"
    (let [{:keys [watch? fresh? cli-fetch]} (parse-main-args [])]
      (is (not watch?))
      (is (not fresh?))
      (is (nil? cli-fetch))))
  (testing "--watch flag"
    (is (:watch? (parse-main-args ["--watch"]))))
  (testing "--fresh flag"
    (is (:fresh? (parse-main-args ["--fresh"]))))
  (testing "-c takes the next arg as config path"
    (is (= "custom.edn" (:config-path (parse-main-args ["-c" "custom.edn"])))))
  (testing "--fetch takes the next arg verbatim"
    (is (= "50" (:cli-fetch (parse-main-args ["--fetch" "50"])))))
  (testing "flags combine regardless of order"
    (let [{:keys [watch? cli-fetch config-path]}
          (parse-main-args ["--watch" "--fetch" "30d" "-c" "x.edn"])]
      (is watch?)
      (is (= "30d" cli-fetch))
      (is (= "x.edn" config-path)))))

(deftest parse-main-args-rejects-dangling-valued-flags
  (testing "--fetch in last position with no value"
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"--fetch requires a value"
                          (parse-main-args ["--fetch"])))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"--fetch requires a value"
                          (parse-main-args ["--watch" "--fetch"]))))
  (testing "-c in last position with no value"
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"-c requires a value"
                          (parse-main-args ["-c"])))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"-c requires a value"
                          (parse-main-args ["--fetch" "50" "-c"])))))

;; ---------------------------------------------------------------------------
;; IMAP watermark advance -- pure prefix computation
;; ---------------------------------------------------------------------------

(deftest max-contiguous-safe-uid-prefix
  (testing "all UIDs safe: watermark reaches the last one"
    (is (= 3 (max-contiguous-safe-uid [1 2 3] #{1 2 3}))))
  (testing "gap in the middle: watermark stops just before it"
    (is (= 2 (max-contiguous-safe-uid [1 2 3 4] #{1 2 4}))))
  (testing "first UID unsafe: watermark cannot advance at all"
    (is (nil? (max-contiguous-safe-uid [1 2 3] #{2 3}))))
  (testing "empty batch: nil"
    (is (nil? (max-contiguous-safe-uid [] #{}))))
  (testing "single safe UID"
    (is (= 7 (max-contiguous-safe-uid [7] #{7})))))

;; ---------------------------------------------------------------------------
;; :mailboxes
;; ---------------------------------------------------------------------------

(def ^:private imap-mb
  {:name "primary" :type :imap :host "imap.example.com"
   :user "me@example.com" :password "secret"})

(def ^:private maildir-mb
  {:name "forge" :type :maildir :path "/tmp/mail/forge"})

(deftest check-mailboxes-test
  (let [error #(:error (main/check-mailboxes %))]
    (testing "a non-empty vector of IMAP and Maildir mailboxes is accepted as is"
      (is (= [imap-mb] (:ok (main/check-mailboxes {:mailboxes [imap-mb]}))))
      (is (nil? (error {:mailboxes [imap-mb]})))
      (is (= [imap-mb maildir-mb] (:ok (main/check-mailboxes {:mailboxes [imap-mb maildir-mb]})))))
    (testing "missing, empty or non-vector :mailboxes"
      (is (re-find #"non-empty vector" (error {})))
      (is (re-find #"non-empty vector" (error {:mailboxes []})))
      (is (re-find #"non-empty vector" (error {:mailboxes imap-mb}))))
    (testing ":name is a non-blank string without slash or colon; spaces are fine"
      (is (re-find #"invalid :name" (error {:mailboxes [(dissoc imap-mb :name)]})))
      (is (re-find #"invalid :name" (error {:mailboxes [(assoc imap-mb :name "   ")]})))
      (is (re-find #"invalid :name" (error {:mailboxes [(assoc imap-mb :name :foo)]})))
      (is (re-find #"invalid :name" (error {:mailboxes [(assoc imap-mb :name "foo/bar")]})))
      (is (re-find #"invalid :name" (error {:mailboxes [(assoc imap-mb :name "foo:bar")]})))
      (is (= [(assoc imap-mb :name "primary mail")]
             (:ok (main/check-mailboxes {:mailboxes [(assoc imap-mb :name "primary mail")]})))))
    (testing ":type is :imap or :maildir"
      (is (re-find #"invalid :type" (error {:mailboxes [(assoc imap-mb :type :pop3)]}))))
    (testing "names are unique"
      (is (re-find #":name values must be unique"
                   (error {:mailboxes [imap-mb (assoc maildir-mb :name "primary")]}))))))

;; ---------------------------------------------------------------------------
;; :ingest override per mailbox -- priority: CLI > local > global > defaults
;; ---------------------------------------------------------------------------

(deftest run-opts-precedence-test
  (testing "global :ingest applies when the mailbox has none; defaults when nothing is set"
    (let [opts (run-opts maildir-mb {:fetch {:limit 200} :max-size 999} nil)]
      (is (= {:limit 200} (select-keys (:fetch-opts opts) [:limit])))
      (is (= 999 (-> opts :ingest-opts :max-size))))
    (is (= {:limit 50} (select-keys (:fetch-opts (run-opts maildir-mb {} nil)) [:limit]))))
  (testing "a mailbox-local key wins; the others fall through to global"
    (let [opts (run-opts (assoc maildir-mb :ingest {:fetch {:limit 5}})
                         {:fetch {:limit 200} :max-size 999} nil)]
      (is (= {:limit 5} (select-keys (:fetch-opts opts) [:limit])))
      (is (= 999 (-> opts :ingest-opts :max-size))))
    (let [opts (run-opts (assoc maildir-mb :ingest {:max-size 100})
                         {:max-size 999 :max-attachment-size 555} nil)]
      (is (= 100 (-> opts :ingest-opts :max-size)))
      (is (= 555 (-> opts :ingest-opts :max-attachment-size)))))
  (testing "a local :fetch wholly replaces the global one (shallow merge)"
    (let [opts (run-opts (assoc maildir-mb :ingest {:fetch {:limit 5}})
                         {:fetch {:since "30d"}} nil)]
      (is (= 5  (-> opts :fetch-opts :limit)))
      (is (nil? (-> opts :fetch-opts :since)))))
  (testing "CLI --fetch is the sole survivor in :fetch-opts"
    (let [opts (run-opts (assoc maildir-mb :ingest {:fetch {:since "30d"}})
                         {:fetch {:limit 200}} {:limit 7})]
      (is (= {:limit 7} (select-keys (:fetch-opts opts) [:limit])))
      (is (nil? (:since (:fetch-opts opts)))))))

(deftest load-context-rejects-missing-config
  (let [missing-path (str (System/getProperty "java.io.tmpdir")
                          "/bone-missing-config-" (System/nanoTime) ".edn")]
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"Config file not found"
                          (load-context nil missing-path)))))

(deftest connection-loss-detection
  (testing "dropped connections, wrapped or not"
    (is (true? (connection-loss? (java.net.SocketException. "reset"))))
    (is (true? (connection-loss?
                (jakarta.mail.MessagingException.
                 "read failed" (java.net.SocketTimeoutException. "timeout")))))
    (is (true? (connection-loss? (jakarta.mail.FolderClosedException. nil)))))
  (testing "a failure on a message is not a connection loss"
    (is (false? (connection-loss? (jakarta.mail.MessagingException. "bad MIME"))))
    (is (false? (connection-loss? (ex-info "boom" {}))))))

(deftest try-digest-skips-an-email-already-digested
  (testing "A thread that loses a concurrent store (upsert) but gets the
            mid after the winner released it does not digest again."
    (let [db-path (str "/tmp/bone-test-" (System/currentTimeMillis))
          conn    (d/get-conn db-path common/bone-schema)
          calls   (atom 0)]
      (try
        (let [{:keys [tempids]} (d/transact! conn [{:db/id -1
                                                    :email/message-id "<td@test.org>"
                                                    :email/digested-at (Date.)}])
              eid (get tempids -1)]
          (with-redefs [digest/process-email! (fn [& _] (swap! calls inc))]
            (is (= :ok ((var-get #'main/try-digest!)
                        {:db-conn conn :source-map {} :sources []}
                        {:db/id eid} "<td@test.org>" nil)))
            (is (zero? @calls) "process-email! must not run again")))
        (finally
          (th/teardown! {:conn conn :db-path db-path}))))))
