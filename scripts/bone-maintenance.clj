#!/usr/bin/env bb

;; bone-maintenance.clj -- Purge orphan emails from the BONE database.
;;
;; An orphan email is one that is not referenced by any report (directly
;; or as a descendant/command), and was not sent by a maintainer.
;;
;; IMPORTANT: Run this only when the JVM daemon is NOT running, to avoid
;; write collisions on the Datalevin database.
;;
;; Usage:
;;   bb maintenance                  -- dry run, show orphan counts
;;   bb maintenance --delete         -- actually delete orphan emails
;;   bb maintenance -n my-source     -- scope to a single source
;;   bb maintenance --verbose        -- list individual orphan message-ids
;;   bb maintenance --failures       -- list recent command failures
;;   bb maintenance --retention DUR  -- orphan retention (default: 90d).
;;                                     Accepts "30d", "6m", "1y" or ISO date.
;;
;; Environment:
;;   BONE_DB -- path to db (default: ./data/bone-db)

(require '[clojure.string :as str]
         '[taoensso.timbre :as log]
         '[bone.common :refer [parse-cutoff-date
                               load-config db-path build-source-map
                               bone-schema format-date
                               failures-file-path read-failures-file
                               reason-labels]]
         '[bone.common-bb :refer [load-datalevin-pod! dq get-tenures]])

;; ---------------------------------------------------------------------------
;; CLI parsing
;; ---------------------------------------------------------------------------

(defn- missing-value!
  "A valued option with no value must abort loudly: silently ignoring
  it would widen the scope of a --delete run (e.g. `--delete -n` with
  the source name forgotten would delete orphans from ALL sources)."
  [opt expected]
  (log/error "Missing value for" opt (str "(expected " expected ")"))
  (System/exit 1))

(defn- parse-args [args]
  (loop [opts {:delete? false :verbose? false :failures? false}
         [a & [v & r :as more]] args]
    (cond
      (nil? a)               opts
      (= a "--delete")       (recur (assoc opts :delete? true) more)
      (= a "--verbose")      (recur (assoc opts :verbose? true) more)
      (= a "--failures")     (recur (assoc opts :failures? true) more)
      (#{"-n" "--source"} a) (if v
                               (recur (assoc opts :source-name v) r)
                               (missing-value! a "a source name"))
      (= a "--retention")    (if v
                               (recur (assoc opts :retention v) r)
                               (missing-value! a "a duration like \"90d\" or an ISO date"))
      :else                  (recur opts more))))

;; ---------------------------------------------------------------------------
;; Protected email collection
;; ---------------------------------------------------------------------------

(defn- report-referenced-eids
  "All email entity IDs reachable from any report via any ref attribute,
  PLUS the emails referenced by qualified relations (:rel/email,
  :rel/retracted-by), series (:series/cover-letter, :series/closed)
  and votes (:vote/email).
  If an email is pointed to by any of these, it's protected."
  [db]
  (let [via-reports (dq '[:find [?e ...]
                          :where
                          [?r :report/type _]
                          [?r _ ?e]
                          [?e :email/message-id _]]
                        db)
        via-attr    (fn [attr]
                      (dq '[:find [?e ...]
                            :in $ ?attr
                            :where
                            [?x ?attr ?e]
                            [?e :email/message-id _]]
                          db attr))]
    (set (concat via-reports
                 (mapcat via-attr [:rel/email :rel/retracted-by
                                   :series/cover-letter :series/closed
                                   :vote/email])))))

(defn- maintainer-addresses
  "Union of all addresses that ever held maintainer status on any source,
  including closed tenures -- these are still 'privileged' for the purpose
  of orphan detection (we don't want to delete emails from a former
  maintainer just because their tenure was closed)."
  [db source-map]
  (->> source-map
       (mapcat (fn [[src-name _]]
                 (keep :email (get-tenures db src-name))))
       (map str/lower-case)
       (into #{})))

(defn- all-emails
  "All email entities as {:eid :source :from :date :mid}.  :from is \"\"
  when the email has no parseable author address."
  [db]
  (->> (dq '[:find ?e ?src ?from ?date ?mid
             :where
             [?e :email/source ?src]
             [(get-else $ ?e :email/author-address "") ?from]
             [?e :email/date-sent ?date]
             [?e :email/message-id ?mid]]
           db)
       (mapv (fn [[eid src from date mid]]
               {:eid eid :source src :from from :date date :mid mid}))))

;; ---------------------------------------------------------------------------
;; Orphan detection
;; ---------------------------------------------------------------------------

(defn- resolve-retention-cutoff
  "Resolve --retention to a cutoff Date (default: 90 days ago).
  Exits with an error on an unparseable value -- called before the DB
  connection is opened, so the exit leaves nothing to clean up."
  [retention]
  (if retention
    (or (parse-cutoff-date retention)
        (do (log/error "Invalid --retention value:" (pr-str retention)
                       "(expected duration like \"90d\", \"6m\", \"1y\""
                       "or ISO date \"yyyy-MM-dd\")")
            (System/exit 1)))
    (java.util.Date. (- (System/currentTimeMillis) (* 90 24 60 60 1000)))))

(defn- find-orphans
  "Returns a seq of {:eid :source :from :date :mid} for orphan emails."
  [db source-map source-name cutoff-date]
  (let [protected   (report-referenced-eids db)
        maintainers (maintainer-addresses db source-map)
        source-ok?  (if source-name
                      #(= (:source %) source-name)
                      (constantly true))
        emails      (all-emails db)]
    (log/info "Total emails in DB:" (count emails))
    (log/info "Protected by reports:" (count protected))
    (log/info "Maintainer addresses:" (count maintainers))
    (log/info "Orphan retention cutoff:" cutoff-date)
    (->> emails
         (remove #(contains? protected (:eid %)))
         (remove #(contains? maintainers (str/lower-case (:from %))))
         (filter source-ok?)
         ;; :email/date-sent is required by the all-emails query.
         (filter #(.before ^java.util.Date (:date %) cutoff-date))
         vec)))

;; ---------------------------------------------------------------------------
;; Command failures
;; ---------------------------------------------------------------------------

(defn- show-failures
  "Display command failures from the failures file, optionally filtered by source."
  [source-name]
  (let [all      (read-failures-file failures-file-path)
        failures (->> all
                      (filter #(or (nil? source-name) (= source-name (:source %))))
                      (sort-by :date #(compare %2 %1)))]
    (if (empty? failures)
      (log/info "No command failures found.")
      (do
        (log/info (count failures) "command failure(s)")
        (doseq [{:keys [from source date reason command]} failures]
          (println (str "  " (format-date date)
                        " | " source
                        " | " from
                        " | " command
                        " -- " (or (get reason-labels reason)
                                   (some-> reason name)
                                   "unknown"))))))))

;; ---------------------------------------------------------------------------
;; Main
;; ---------------------------------------------------------------------------

(load-datalevin-pod!)

(let [{:keys [delete? verbose? failures? source-name retention]}
      (parse-args *command-line-args*)
      config  (load-config)
      _       (when-not config
                (log/error "No config.edn found")
                (System/exit 1))
      dbp     (db-path config)
      source-map (build-source-map config)
      _       (when (and source-name (not (contains? source-map source-name)))
                (log/error "Unknown source:" source-name)
                (log/error "Available:" (str/join ", " (keys source-map)))
                (System/exit 1))]
  (if failures?
    ;; --failures only reads the failures EDN file -- no DB connection
    ;; (nor retention cutoff) needed, so the daemon can keep running.
    (do (when retention
          (log/warn "--retention is ignored with --failures"))
        (show-failures source-name))
    (let [;; Validated before the connection opens: System/exit here must
          ;; not skip the (finally (d/close conn)) below.
          cutoff (resolve-retention-cutoff retention)
          ;; Open with WAL for potential writes
          conn   (d/get-conn dbp bone-schema {})]
      (try
        (let [db      (d/db conn)
              orphans (find-orphans db source-map source-name cutoff)]
          (if (empty? orphans)
            (log/info "No orphan emails found.")
            (let [by-source (group-by :source orphans)]
              (log/info "Found" (count orphans) "orphan email(s)")
              (doseq [[src os] (sort-by key by-source)]
                (log/info (str "  [" src "] " (count os) " orphan(s)")))
              (when verbose?
                (doseq [{:keys [source mid from date]} (sort-by :date orphans)]
                  (println (str "  " source " | " mid " | " from " | " date))))
              (if delete?
                (do
                  (log/info "Deleting" (count orphans) "orphan email(s)...")
                  (let [tx-data (mapv (fn [{:keys [eid]}]
                                        [:db/retractEntity eid])
                                      orphans)]
                    (d/transact! conn tx-data)
                    (log/info "Done. Deleted" (count orphans) "email(s).")))
                (do
                  (log/info "Dry run -- no changes made. Pass --delete to remove.")
                  (log/info "Tip: use --verbose to list individual orphan message-ids."))))))
        (finally
          (d/close conn))))))
