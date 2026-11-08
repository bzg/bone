#!/usr/bin/env bb

;; bone-notify.clj -- Send notification emails to subscribers.
;;
;; BONE: Backlog Of Notable Emails
;;
;; Reads the subscriber list from config.edn (:notifications
;; :subscribers), builds plain-text summaries from the DB (read-only),
;; and sends emails via SMTP.  The cadence is the operator's
;; responsibility: schedule bb notify from cron or a systemd timer.
;;
;; Operational state lives in data/.last-notify-failures.edn -- a
;; per-(subscriber, source) timestamp of the last successful send,
;; used to avoid re-mailing the same failures.  Delete the file to
;; replay all failures on the next run.
;;
;; Usage:
;;   bb notify           -- send notifications
;;   bb notify --dry-run -- show what would be sent without sending
;;   bb notify --debug   -- verbose diagnostics

(ns bone-notify
  (:require [babashka.pods :as pods]
            [clojure.string :as str]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [taoensso.timbre :as log]
            [bone.common :refer [get-header-values format-date-iso
                                    report-priority bone-schema]]
            [bone.config :refer [load-config db-path build-source-map]]
            [bone.export :refer [format-date report-descendant-count reason-labels]]
            [bone.common-bb :refer [load-datalevin-pod! all-reports]]))

(load-datalevin-pod!)
(pods/load-pod 'tzzh/mail "0.0.3")

(require '[pod.tzzh.mail :as mail])

;; ---------------------------------------------------------------------------
;; Per-(subscriber, source) failures-shown tracking: timestamp of the
;; last successful notification, so failures are not re-sent on every
;; run.  Operational state, not configuration.

(def ^:private last-failures-file "data/.last-notify-failures.edn")

(defn- load-last-failures-shown
  "Read {key -> epoch-millis} from the state file, or {}.  A corrupt
  file would re-spam every subscriber with old failures, so log at warn
  before returning the empty default."
  []
  (let [f (io/file last-failures-file)]
    (if (.exists f)
      (try (edn/read-string (slurp f))
           (catch Exception e
             (log/warn "Could not parse" last-failures-file
                       "-- starting from empty state.  Subscribers may"
                       "see previously-shown failures on this run."
                       (.getMessage e))
             {}))
      {})))

(defn- save-last-failures-shown!
  [m]
  (io/make-parents last-failures-file)
  (spit last-failures-file (pr-str m)))

(defn- failures-key
  "Key under which we track the last-shown timestamp for a subscription."
  [email source]
  (str source ":" (str/lower-case email)))

;; ---------------------------------------------------------------------------
;; Failure queries
;; ---------------------------------------------------------------------------

(def ^:private failure-pull
  '[:failure/source :failure/from :failure/date :failure/recorded-at
    :failure/reason :failure/command :failure/audience
    {:failure/report [{:report/email [:email/subject]}]}])

(defn- db-created-at
  "When the database was first opened (:meta/created-at), or nil."
  [db]
  (some->> (d/entid db [:meta/ident "global"])
           (d/pull db [:meta/created-at])
           :meta/created-at))

(defn- load-failures
  "The command failures whose email postdates the database.  Those
  from before are history -- mailed by the previous incarnation, or
  nobody's business -- so a --fresh replay mails nothing.  Enumerated
  on :failure/recorded-at, never by a d/q on the unique key."
  [db]
  (let [^java.util.Date born (db-created-at db)]
    (->> (d/q (list :find (list 'pull '?f failure-pull)
                    :where '[?f :failure/recorded-at])
              db)
         (map first)
         (filter (fn [{:failure/keys [^java.util.Date date]}]
                   (or (nil? born) (and date (.after date born))))))))

(defn- failures-for-subscriber
  "Return failures on `:source` routed to `:email`, recorded after
  `:since` (or all of them if `:since` is nil).

  Routing is driven by :failure/audience:
  - `:author`      -- shown only to the address that triggered the
                     failure (someone seeing their own typo); default
                     when the field is absent.
  - `:maintainers` -- shown to every subscriber on the source."
  [all-failures {:keys [email source since]}]
  (let [addr (str/lower-case email)]
    (->> all-failures
         (filter (fn [{:failure/keys [from audience ^java.util.Date recorded-at]
                       src :failure/source}]
                   (and (= source src)
                        (or (nil? since)
                            ;; Watermark on the wall-clock :failure/recorded-at,
                            ;; not :failure/date (the email's Date: header): a
                            ;; delayed email must still be notified.
                            (.after recorded-at since))
                        (case (or audience :author)
                          :author      (= addr from)
                          :maintainers true
                          false)))))))

;; ---------------------------------------------------------------------------
;; Report formatting
;; ---------------------------------------------------------------------------

(def ^:private actionable-types #{:bug :patch :request})

(defn- open? [report]
  (nil? (:report/closed report)))

(defn- owned-by? [report email]
  (when-let [owner (:report/owned-address report)]
    (= (str/lower-case owner) (str/lower-case email))))

(defn- unacked? [report]
  (nil? (:report/acked report)))

(defn- unowned? [report]
  (nil? (:report/owned report)))

(defn- format-failure-line
  "Format a single command failure as a text line."
  [{:failure/keys [date reason command from report]}]
  (str "  [" (format-date date) "] " command " -- "
       (get reason-labels reason (some-> reason name)) "\n"
       "    by: " from "\n"
       "    on: " (or (-> report :report/email :email/subject) "(no report)")))

(defn- format-report-line
  "Format a single report as a text line."
  [report]
  (let [email   (:report/email report)
        type    (name (:report/type report))
        subject (or (:email/subject email) "(no subject)")
        date    (format-date (:email/date-sent email))
        from    (or (:email/author-address email) "?")
        pri     (report-priority report)
        replies (report-descendant-count report)
        deadline (:report/deadline-value report)
        expiry   (:report/expiry-value report)
        arch    (first (get-header-values (:email/headers-edn email) "Archived-At"))]
    (str "  [" type "] " subject "\n"
         "    from: " from " -- " date
         " -- priority:" pri " replies:" replies
         (when deadline (str " deadline:" (format-date-iso deadline)))
         (when expiry (str " expiry:" (format-date-iso expiry)))
         (when arch (str "\n    " arch)))))

(defn- section
  "Build a text section with a header and report lines, or nil if empty."
  [title reports]
  (when (seq reports)
    (str title "\n"
         (str/join "\n\n" (map format-report-line reports))
         "\n")))

(defn- filter-relevant-reports
  "Filter the full report set against scope filters: actionable type,
  open, on-source, plus optional :subject-match / :topic substrings."
  [reports {:keys [source subject-match topic]}]
  (let [subject-lc (some-> subject-match str/lower-case)
        topic-lc   (some-> topic str/lower-case)]
    (cond->> (->> reports
                  (filter #(contains? actionable-types (:report/type %)))
                  (filter open?)
                  (filter #(= source (get-in % [:report/email :email/source]))))
      subject-lc (filter #(some-> (get-in % [:report/email :email/subject])
                                  str/lower-case
                                  (str/includes? subject-lc)))
      topic-lc   (filter #(some-> (:report/topic-value %)
                                  str/lower-case
                                  (str/includes? topic-lc))))))

(defn- build-sections
  "Group relevant reports into the three body sections.  Sections 1
  and 2 (owned by you, with or without deadline) always show what you
  own.  Section 3 (unacked & unowned) is the noisy one and is the
  only one filtered, by min-priority."
  [relevant email {:keys [min-priority]}]
  (let [owned (filter #(owned-by? % email) relevant)]
    {:dl      (->> owned
                   (filter :report/deadline-value)
                   (sort-by #(.getTime ^java.util.Date (:report/deadline-value %))))
     :owned   (->> owned
                   (remove :report/deadline-value)
                   (sort-by #(- (report-priority %))))
     :unacked (->> relevant
                   (filter unacked?)
                   (filter unowned?)
                   (filter #(>= (report-priority %) min-priority)))}))

(defn- failures-section
  "Build the failures section text, or nil when there are no failures."
  [source failures]
  (when (seq failures)
    (str "== Failed commands (" source ") ==\n"
         (str/join "\n\n" (map format-failure-line failures))
         "\n")))

(defn- join-sections
  "Concatenate the non-nil sections with blank-line separators and
  append a short footer.  Returns nil when every section is nil."
  [sections]
  (let [present (filter some? sections)]
    (when (seq present)
      (str (str/join "\n" present)
           "\n-- \nSent by BONE -- reply to change your subscription"))))

(defn build-email-body
  "Build the notification email body for one (email, subscription) pair.
  `failures` is a seq of :failure/* entities to include."
  [reports email subscription failures]
  (let [source   (:source subscription)
        prefs    (merge {:min-priority 1} subscription)
        relevant (->> (filter-relevant-reports reports prefs)
                      (sort-by (juxt report-priority report-descendant-count)
                               #(compare %2 %1)))
        {:keys [dl owned unacked]} (build-sections relevant email prefs)]
    (log/debug "build-email-body for" email (str "(source: " source ")"))
    (log/debug "  total reports:" (count reports) "-- relevant:" (count relevant))
    (join-sections
     [(failures-section source failures)
      (section (str "== Upcoming deadlines -- owned by you (" source ") ==") dl)
      (section (str "== Open bugs/patches/requests owned by you (" source ") ==") owned)
      (section (str "== Unacked & unowned bugs/patches/requests (" source ") ==") unacked)])))

;; ---------------------------------------------------------------------------
;; Per-source kill switch
;; ---------------------------------------------------------------------------

(defn- source-notify-enabled?
  "True unless the source explicitly sets :notifications {:enabled false}."
  [source-map source-name]
  (let [src-cfg (get source-map source-name)]
    (not (false? (get-in src-cfg [:notifications :enabled])))))

;; ---------------------------------------------------------------------------
;; SMTP
;; ---------------------------------------------------------------------------

(defn- bcc-list
  "Normalize `:admin-bcc` to a vector of addresses (or nil when absent).
  Accepts a single string or a vector of strings."
  [admin-bcc]
  (cond
    (nil? admin-bcc)         nil
    (string? admin-bcc)      [admin-bcc]
    (sequential? admin-bcc)  (vec admin-bcc)
    :else                    (do (log/warn "Ignoring invalid :admin-bcc"
                                           (pr-str admin-bcc))
                                 nil)))

(defn send-notification!
  "Send a plain-text notification email via SMTP.
  `admin-bcc` is forwarded as :bcc on every send when non-nil.

  Note: the tzzh/mail pod expects the unhyphenated key
  `:replyto` (and as a vector of strings); `:reply-to` would be
  silently dropped."
  [smtp-config to-addr source body admin-bcc]
  (let [{:keys [host port tls user password from reply-to]} smtp-config
        bccs (bcc-list admin-bcc)]
    (mail/send-mail
     (cond-> {:host     host
              :port     port
              :tls      (boolean tls)
              :username user
              :password password
              :from     from
              :to       [to-addr]
              :subject  (str "[BONE " source "] Open reports")
              :text     body}
       reply-to    (assoc :replyto [reply-to])
       (seq bccs)  (assoc :bcc bccs)))))

;; ---------------------------------------------------------------------------
;; Main
;; ---------------------------------------------------------------------------

(defn- log-skipped-pairs!
  "Debug-log subscribers skipped because their source has
  :notifications {:enabled false}."
  [src-map pairs]
  (doseq [[email s] pairs
          :when (not (source-notify-enabled? src-map (:source s)))]
    (log/debug "SKIPPED" email
               "-- notifications disabled for source" (:source s))))

(defn -main [& args]
  (let [flags       (set args)
        dry-run?    (flags "--dry-run")
        debug?      (flags "--debug")
        _           (when debug? (log/merge-config! {:min-level :debug}))
        config      (load-config)
        dbp         (db-path config)
        notif       (:notifications config)
        smtp        (:smtp notif)
        admin-bcc   (:admin-bcc notif)
        subscribers (:subscribers notif)]
    (cond
      ;; Fail the cron job, not pass for "notifications disabled".
      (nil? config)
      (do (log/error "Config file not found or empty:"
                     (or (System/getenv "BONE_CONFIG") "config.edn"))
          (System/exit 1))

      (not (:enabled notif))
      (do (log/info "Notifications disabled in config.")
          (System/exit 0))

      (nil? smtp)
      (do (log/error "No :smtp config under :notifications.")
          (System/exit 1))

      (empty? subscribers)
      (do (log/info "No :subscribers configured.")
          (System/exit 0))

      :else
      (let [conn          (d/get-conn dbp bone-schema {:wal? false})
            last-shown    (load-last-failures-shown)
            updated-shown (atom last-shown)
            sent          (atom 0)
            failed        (atom 0)]
        (try
          (let [db           (d/db conn)
                src-map      (build-source-map config)
                reports      (all-reports db)
                ;; Watermark candidate, captured BEFORE reading the
                ;; failures: one recorded by the daemon between the
                ;; read and the send may be shown twice on the next
                ;; run, but is never lost (storing the send time
                ;; instead would skip it forever).
                run-started  (java.util.Date.)
                all-failures (load-failures db)
                pairs        (for [[email subs] subscribers
                                   s             subs]
                               [email s])
                live-pairs   (filter (fn [[_ s]] (source-notify-enabled? src-map (:source s))) pairs)]
            (log/debug (count pairs) "subscription(s) configured")
            (log-skipped-pairs! src-map pairs)
            (log/debug (count live-pairs) "after per-source filter")
            (if (empty? live-pairs)
              (log/info "No active subscriptions.")
              (doseq [[email subscription] live-pairs]
                (let [source   (:source subscription)
                      k        (failures-key email source)
                      since-ms (get last-shown k)
                      since    (when since-ms (java.util.Date. (long since-ms)))
                      failures (failures-for-subscriber
                                all-failures
                                {:email email :source source :since since})
                      body     (build-email-body reports email subscription failures)]
                  (if body
                    (do (log/info (if dry-run? "[dry-run]" "")
                                  "Notifying" email (str "(source: " source ")"))
                        (when-not dry-run?
                          (try
                            (send-notification! smtp email source body admin-bcc)
                            (swap! updated-shown assoc k (.getTime run-started))
                            (swap! sent inc)
                            (catch Exception e
                              ;; Don't advance the timestamp on failure so the
                              ;; same failures are retried next run.
                              (swap! failed inc)
                              (log/error "Failed to send to" email
                                         (str "(source: " source "):")
                                         (.getMessage e)))))
                        (when dry-run?
                          (println "---")
                          (println body)
                          (println "---")))
                    (log/info "No open items for" email
                              (str "(source: " source "),") "skipping.")))))
            (log/info "Done." (if dry-run? "Dry run, no emails sent." (str @sent " email(s) sent."))))
          (finally
            (when-not dry-run?
              (try (save-last-failures-shown! @updated-shown)
                   (catch Exception e
                     (log/error "Failed to persist last-notify-failures:"
                                (.getMessage e)))))
            (try (d/close conn) (catch Exception _ nil))))
        ;; After the finally above has saved the state.
        (when (pos? @failed)
          (log/error @failed "notification(s) could not be sent.")
          (System/exit 1))))))

;; Run only when invoked as a script, not when the file is loaded.
(when (= (System/getProperty "babashka.file") *file*)
  (apply -main *command-line-args*))
