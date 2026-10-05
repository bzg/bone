;; Copyright (c) 2026 Bastien Guerry <bzg@gnu.org>
;; SPDX-License-Identifier: EPL-2.0
;; License-Filename: LICENSES/EPL-2.0.txt

(ns bone.series
  "Patch series management."
  (:require [clojure.string :as str]
            [datalevin.core :as d]
            [bone.relations :as rel]
            [bone.tracking :as tracking]
            [taoensso.timbre :as log])
  (:import [java.util Date]))

(defn parse-seq
  "Parse \"2/5\" into [2 5], or nil."
  [s]
  (when s
    (let [[_ n m] (re-find #"(\d+)/(\d+)" s)]
      (when (and n m) [(parse-long n) (parse-long m)]))))

(defn series-id [topic sender total]
  (str (or topic "") "|" sender "|" total))

(defn next-sid
  "Pure: the next unused series id for `base` given the `existing`
  ids -- `base` itself when free, else \"base#N\" one past the highest
  suffix.  \"base\" counts as suffix 1; only exact-base or base#N ids
  count: the caller's starts-with? prefetch also matches longer totals
  (\"t|s|5\" matches \"t|s|55#3\"), so the boundary is re-checked here."
  [base existing]
  (let [suffix-n (fn [sid]
                   (cond
                     (= sid base) 1
                     (str/starts-with? sid (str base "#"))
                     (some-> (re-find #"#(\d+)$" sid) second parse-long)))
        max-n    (reduce max 0 (keep suffix-n existing))]
    (if (zero? max-n) base (str base "#" (inc max-n)))))

(defn- next-series-id [db topic sender total]
  (let [base (series-id topic sender total)]
    (next-sid base (d/q '[:find [?sid ...]
                          :in $ ?prefix
                          :where [?s :series/id ?sid]
                          [(clojure.string/starts-with? ?sid ?prefix)]]
                        db base))))

(defn find-open-series [db topic sender total]
  (d/q '[:find ?s .
         :in $ ?topic ?sender ?exp
         :where
         [?s :series/topic ?topic] [?s :series/sender ?sender]
         [?s :series/expected ?exp] (not [?s :series/closed _])]
       db topic sender total))

(defn find-open-series-by-topic-sender [db topic sender]
  (when (and topic sender)
    (d/q '[:find [?s ...]
           :in $ ?topic ?sender
           :where
           [?s :series/topic ?topic] [?s :series/sender ?sender]
           (not [?s :series/closed _])]
         db topic sender)))

(defn find-open-series-via-parents
  "Open series reachable via parents' :report/series, restricted to
  `sender`/`total`.  Threading is more reliable than per-message
  :topic, which can diverge across patches in a series."
  [db parent-report-eids sender total]
  (when (seq parent-report-eids)
    (d/q '[:find ?s .
           :in $ [?r ...] ?sender ?total
           :where
           [?r :report/series ?s]
           [?s :series/sender ?sender]
           [?s :series/expected ?total]
           (not [?s :series/closed _])]
         db parent-report-eids sender total)))

(defn create-series! [conn topic sender total]
  (let [sid (next-series-id (d/db conn) topic sender total)]
    (d/transact! conn [{:series/id       sid
                        :series/topic    (or topic "")
                        :series/sender   sender
                        :series/expected total}])
    (d/entid (d/db conn) [:series/id sid])))

(defn close-series! [conn series-eid email-eid]
  (d/transact! conn [{:db/id series-eid :series/closed email-eid}]))

(defn- supersede-series-reports!
  "Close all open reports in a superseded series with reason :superseded.
  One transaction + one bump for the whole batch."
  [conn series-eid email-eid]
  (let [db (d/db conn)
        report-eids (d/q '[:find [?r ...]
                           :in $ ?s
                           :where [?s :series/patches ?r]
                           (not [?r :report/closed _])]
                         db series-eid)]
    (when (seq report-eids)
      (d/transact! conn (mapv (fn [rid]
                                {:db/id rid
                                 :report/closed email-eid
                                 :report/close-reason :superseded})
                              report-eids))
      (tracking/bump-report-updated! conn report-eids))))

(defn add-patch-to-series! [conn series-eid report-eid email]
  (let [existing (d/q '[:find [?r ...]
                         :in $ ?s ?self
                         :where [?s :series/patches ?r]
                         [(not= ?r ?self)]]
                       (d/db conn) series-eid report-eid)]
    (d/transact! conn [[:db/add series-eid :series/patches report-eid]
                       {:db/id report-eid :report/series series-eid}])
    ;; Cross-link siblings via :related-to (idempotent via :rel/id).
    ;; Siblings that gain a link must re-export -- contextual change
    ;; (a new patch joined the series), not a state change.
    (let [opts {:from-eid  report-eid
                :kind      :related-to
                :setter    (:email/author-address email)
                :email-eid (:db/id email)
                :posed-at  (or (:email/date-sent email) (Date.))}
          linked (filterv (fn [sibling-eid]
                            (rel/pose-if-absent! conn (assoc opts :to-eid sibling-eid)))
                          existing)]
      (when (seq linked)
        (tracking/bump-report-updated! conn linked false)))))

(defn set-cover-letter! [conn series-eid email-eid]
  (d/transact! conn [{:db/id series-eid :series/cover-letter email-eid}]))

(defn- series-restart-info
  "DB read feeding `series-restart-plan`: describe the open series
  `series-eid` as {:eid :seqs :mids :cover-mid :empty?}."
  [db series-eid]
  (let [{patches :series/patches cover :series/cover-letter}
        (d/pull db [{:series/patches [:report/patch-seq :report/message-id]}
                    {:series/cover-letter [:email/message-id]}]
                series-eid)]
    {:eid       series-eid
     :seqs      (into #{} (keep :report/patch-seq) patches)
     :mids      (into #{} (keep :report/message-id) patches)
     :cover-mid (:email/message-id cover)
     :empty?    (empty? patches)}))

(defn series-restart-plan
  "Pure: the series eids a numbered patch closes as an implicit
  restart, or nil when it closes none.  `n` is the patch's sequence
  number, `existing` the sender's open same-topic series as
  `series-restart-info` maps, `parent-mids` the mids of the reports
  the patch replies to.  A restart is a cover (0/N), or a 1/N when
  some existing series already holds a 1/...; it must thread back to an
  old series (a patch or cover mid among `parent-mids`).  A numbered
  patch never closes the empty series its own cover letter just
  opened: that series is the one it is joining, not an old revision
  to supersede."
  [n existing parent-mids]
  (let [restart? (or (zero? n)
                     (and (= 1 n)
                          (some (fn [s]
                                  (some #(str/starts-with? % "1/") (:seqs s)))
                                existing)))
        old-mids (into (set (mapcat :mids existing))
                       (keep :cover-mid)
                       existing)]
    (when (and (seq existing) restart? (some old-mids parent-mids))
      (into []
            (comp (remove (fn [s]
                            (and (pos? n)
                                 (:empty? s)
                                 (contains? parent-mids (:cover-mid s)))))
                  (map :eid))
            existing))))

(defn manage-series!
  "After creating a patch report, manage its series membership."
  [conn report-eid email report-info from-addr parent-report-eids]
  (when-let [[n m] (parse-seq (:patch-seq report-info))]
    (let [email-eid (:db/id email)
          topic  (:topic report-info)
          db     (d/db conn)
          existing-series (find-open-series-by-topic-sender db topic from-addr)
          parent-mids (when (and (seq existing-series) (seq parent-report-eids))
                        (set (d/q '[:find [?mid ...]
                                    :in $ [?r ...]
                                    :where [?r :report/message-id ?mid]]
                                  db parent-report-eids)))
          to-close (series-restart-plan
                    n
                    (mapv #(series-restart-info db %) existing-series)
                    (or parent-mids #{}))]
      (doseq [sid to-close]
        (close-series! conn sid email-eid)
        (when (:version report-info)
          (supersede-series-reports! conn sid email-eid))
        (log/info "Auto-closed series"
                  (pr-str (:series/id (d/pull (d/db conn) [:series/id] sid)))
                  "(superseded)"))
      (let [series-eid (or (when (pos? n)
                             (find-open-series-via-parents
                              (d/db conn) parent-report-eids from-addr m))
                           ;; No fallback on the "" topic key: it would
                           ;; merge unrelated threads from the same sender.
                           ;; Threading (above) is the grouping mechanism;
                           ;; without a topic, create a fresh series.
                           (when topic
                             (find-open-series (d/db conn) topic from-addr m))
                           (let [sid (create-series! conn topic from-addr m)]
                             ;; Log the allocated :series/id, not the base
                             ;; id: next-series-id may have suffixed #N.
                             (log/info "New series:"
                                       (pr-str (:series/id
                                                (d/pull (d/db conn) [:series/id] sid)))
                                       "(expecting" m "patches)")
                             sid))]
        ;; Series supersession is implicit: the old series carries
        ;; :series/closed pointing to the triggering email; the new
        ;; series lives in the same thread.
        (if (zero? n)
          (do (set-cover-letter! conn series-eid email-eid)
              (d/transact! conn [{:db/id report-eid :report/series series-eid}]))
          (add-patch-to-series! conn series-eid report-eid email))))))
