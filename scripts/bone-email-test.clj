#!/usr/bin/env bb

;; bone-email-test.clj -- Test SMTP configuration by sending a test email.
;;
;; Reads :notifications :smtp from config.edn, sends a short test message
;; to the lead maintainer of the first source (or to a custom address via --to).
;;
;; Usage:
;;   bb test-smtp                    -- validate config, no email sent (default)
;;   bb test-smtp --send             -- actually send the test email
;;   bb test-smtp --to me@example.com --send -- send to a specific address

(require '[babashka.pods :as pods]
         '[clojure.string :as str]
         '[bone.common :refer [load-config]])

;; ---------------------------------------------------------------------------
;; Config validation
;; ---------------------------------------------------------------------------

(defn- check-key [m k label]
  (when-not (get m k)
    (println (str "  ✗ Missing " label " (" (name k) ")"))
    true))

(defn validate-smtp-config
  "Validate that all required SMTP keys are present. Returns true if valid."
  [smtp]
  (let [required [[:host "SMTP host"]
                  [:port "SMTP port"]
                  [:user "SMTP user"]
                  [:password "SMTP password"]
                  [:from "From address"]]
        errors (keep (fn [[k label]] (check-key smtp k label)) required)]
    (if (seq errors)
      (do (println (str "\n" (count errors) " missing SMTP field(s)."))
          false)
      (do (println "  ✓ All required SMTP fields present")
          (println (str "    host: " (:host smtp)))
          (println (str "    port: " (:port smtp)))
          (println (str "    tls:  " (boolean (:tls smtp))))
          (println (str "    user: " (:user smtp)))
          (println (str "    from: " (:from smtp)))
          true))))

;; ---------------------------------------------------------------------------
;; Main
;; ---------------------------------------------------------------------------

(when (= (System/getProperty "babashka.file") *file*)
  (let [args     (vec *command-line-args*)
        send?    (some #{"--send"} args)
        to-idx   (.indexOf args "--to")
        to-arg   (when (nat-int? to-idx) (get args (inc to-idx)))
        config   (load-config)]

    (println "== BONE SMTP test ==\n")

    ;; --to without a value must not fall back to the lead maintainer:
    ;; with --send the test email would go to the wrong recipient.
    (when (and (nat-int? to-idx)
               (or (nil? to-arg) (str/starts-with? to-arg "--")))
      (println "✗ --to requires an address (e.g. --to me@example.com).")
      (System/exit 1))

    ;; --- Check config.edn ---
    (when-not config
      (println "✗ config.edn not found.")
      (System/exit 1))

    (let [notif (:notifications config)]
      (when-not notif
        (println "✗ No :notifications key in config.edn.")
        (System/exit 1))
      (when-not (:enabled notif)
        (println "⚠ Notifications are disabled (:enabled false). Testing anyway."))

      ;; --- Validate SMTP fields ---
      (println "Checking SMTP config...")
      (let [smtp (:smtp notif)]
        (when-not smtp
          (println "✗ No :smtp key under :notifications.")
          (System/exit 1))
        (when-not (validate-smtp-config smtp)
          (System/exit 1))

        ;; --- Resolve recipient: --to <addr>, else the lead maintainer
        ;; (first entry of the first source's :maintainers). ---
        (let [to-addr (or to-arg
                          (some-> config :sources first :maintainers first))]
          (when-not to-addr
            (println "\n✗ No recipient: add a :maintainers entry on the first source,"
                     "or use --to <addr>.")
            (System/exit 1))

          (println (str "\nRecipient: " to-addr))

          (if-not send?
            (println "\n✓ Config looks good.  Re-run with --send to actually send the test email.")

            ;; --- Send test email ---
            (do
              (println "Sending test email...")
              (pods/load-pod 'tzzh/mail "0.0.3")
              (require 'pod.tzzh.mail)

              (let [body (str "This is a test email from BONE.\n\n"
                              "If you received this, your SMTP configuration is working.\n\n"
                              "Timestamp: " (java.util.Date.) "\n"
                              "Host: " (:host smtp) "\n"
                              "From: " (:from smtp) "\n")]
                (try
                  ((resolve 'pod.tzzh.mail/send-mail)
                   {:host     (:host smtp)
                    :port     (:port smtp)
                    :tls      (boolean (:tls smtp))
                    :username (:user smtp)
                    :password (:password smtp)
                    :from     (:from smtp)
                    :to       [to-addr]
                    :subject  "[BONE] SMTP test"
                    :text     body})
                  (println (str "\n✓ Test email sent to " to-addr "."))
                  (catch Exception e
                    (println (str "\n✗ Failed to send: " (or (.getMessage e) (pr-str e))))
                    (System/exit 1)))))))))))
