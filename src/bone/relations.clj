;; Copyright (c) 2026 Bastien Guerry <bzg@gnu.org>
;; SPDX-License-Identifier: EPL-2.0
;; License-Filename: LICENSES/EPL-2.0.txt

(ns bone.relations
  "Qualified relations between reports.  Pure helpers first (IDs,
  validation, tx-builders), then IO helpers that pose/retract.
  Asymmetric kinds (:resolves, :supersedes, :duplicates) store two
  datoms (one per direction); :related-to stores one canonicalized
  by ascending eid order.  See bone-schema.edn for the :rel/* attrs."
  (:require [clojure.string :as str]
            [datalevin.core :as d]
            [bone.tracking :as tracking]
            [taoensso.timbre :as log]))

(def all-kinds
  #{:resolves :resolved-by
    :supersedes :superseded-by
    :duplicates :duplicated-by
    :related-to})

(def asymmetric-kinds
  "Kinds stored as two datoms; they also require both ends to be
  actionable (:bug, :patch, :request)."
  #{:resolves :resolved-by
    :supersedes :superseded-by
    :duplicates :duplicated-by})

(def symmetric-kinds #{:related-to})

(def inverse-kinds
  {:resolves      :resolved-by
   :resolved-by   :resolves
   :supersedes    :superseded-by
   :superseded-by :supersedes
   :duplicates    :duplicated-by
   :duplicated-by :duplicates
   :related-to    :related-to})

(def same-type-kinds
  "Kinds requiring (:report/type from) == (:report/type to)."
  #{:supersedes :superseded-by :duplicates :duplicated-by})

(def actionable-types #{:bug :patch :request})

(defn make-relation-id
  [from-eid kind to-eid]
  (str from-eid ":" (name kind) ":" to-eid))

(defn canonicalize
  "[from to] in ascending eid order, so reciprocal poses of a
  symmetric kind produce the same :rel/id."
  [from-eid to-eid]
  (if (neg? (compare to-eid from-eid))
    [to-eid from-eid]
    [from-eid to-eid]))

(defn- valid-resolves-pair?
  "True iff source/target form a legal :resolves pose (patch resolves
  bug or request).  :resolved-by is checked by inverting the pair."
  [source-type target-type]
  (and (= source-type :patch)
       (contains? #{:bug :request} target-type)))

(defn valid-pose?
  "True when a relation of `kind` is legal between these reports
  (kind known, no self-loop, type constraints satisfied)."
  [kind source-eid target-eid source-type target-type]
  (boolean
   (and (all-kinds kind)
        (not= source-eid target-eid)
        (or (not (asymmetric-kinds kind))
            (and (actionable-types source-type)
                 (actionable-types target-type)))
        (or (not (same-type-kinds kind))
            (= source-type target-type))
        (case kind
          :resolves    (valid-resolves-pair? source-type target-type)
          :resolved-by (valid-resolves-pair? target-type source-type)
          true))))

(defn pose-tx
  "Datoms to pose a relation: 2 entity maps for asymmetric kinds, 1
  for symmetric.  Caller must have validated via `valid-pose?`; go
  through `pose-if-absent!` for idempotence and reactivation.
  `value` is optional."
  [{:keys [from-eid to-eid kind setter email-eid posed-at value]}]
  (let [mk-rel (fn [f t k]
                 (cond-> {:rel/id       (make-relation-id f k t)
                          :rel/from     f
                          :rel/to       t
                          :rel/kind     k
                          :rel/setter   (some-> setter str/lower-case)
                          :rel/email    email-eid
                          :rel/posed-at posed-at
                          :rel/active?  true}
                   value (assoc :rel/value value)))]
    (if (symmetric-kinds kind)
      (let [[f t] (canonicalize from-eid to-eid)]
        [(mk-rel f t kind)])
      [(mk-rel from-eid to-eid kind)
       (mk-rel to-eid from-eid (inverse-kinds kind))])))

(defn retract-tx
  "Datoms to mark a relation retracted (active? false + audit ref)."
  [rel-eid retracted-by-email-eid]
  [{:db/id            rel-eid
    :rel/active?      false
    :rel/retracted-by retracted-by-email-eid}])

(defn paired-relation-ids
  "Pair of :rel/id strings for both directions of an asymmetric kind,
  or single canonical id for a symmetric one."
  [kind from-eid to-eid]
  (if (symmetric-kinds kind)
    (let [[f t] (canonicalize from-eid to-eid)]
      [(make-relation-id f kind t)])
    [(make-relation-id from-eid kind to-eid)
     (make-relation-id to-eid (inverse-kinds kind) from-eid)]))

;; ---------------------------------------------------------------------------
;; IO helpers (require datalevin)
;; ---------------------------------------------------------------------------

(defn- reactivate-tx
  "Datoms to reactivate a retracted relation under a new pose: the
  re-poser becomes the setter, the retraction audit is cleared."
  [{eid :db/id retracted-by :rel/retracted-by} {:keys [setter email-eid posed-at value]}]
  (cond-> [(cond-> {:db/id eid :rel/active? true}
             setter    (assoc :rel/setter (str/lower-case setter))
             email-eid (assoc :rel/email email-eid)
             posed-at  (assoc :rel/posed-at posed-at)
             value     (assoc :rel/value value))]
    (:db/id retracted-by)
    (conj [:db/retract eid :rel/retracted-by (:db/id retracted-by)])))

(defn pose-if-absent!
  "Pose a relation, idempotently via :rel/id.  Active relation: no-op.
  Retracted relation: reactivated iff the pose is dated after the
  retraction (so `Superseded-by:` after `Not superseded-by.` works,
  but an out-of-order re-digest of an old pose cannot undo a later
  retract).  Returns the tx result when something changed, else nil."
  [conn opts]
  (let [db   (d/db conn)
        ids  (paired-relation-ids (:kind opts) (:from-eid opts) (:to-eid opts))
        rels (keep (fn [id]
                     (when-let [e (d/entid db [:rel/id id])]
                       (d/pull db [:db/id :rel/active?
                                   {:rel/retracted-by [:db/id :email/date-sent]}]
                               e)))
                   ids)]
    (cond
      (empty? rels)
      (d/transact! conn (pose-tx opts))

      (some :rel/active? rels)
      nil

      :else
      ;; (first rels) suffices: both directions of a pair are always
      ;; retracted together, sharing the same :rel/retracted-by email.
      (let [^java.util.Date posed-at     (:posed-at opts)
            ^java.util.Date retracted-at (some-> (first rels)
                                                 :rel/retracted-by
                                                 :email/date-sent)]
        (when (or (nil? posed-at) (nil? retracted-at)
                  (.after posed-at retracted-at))
          (d/transact! conn (into [] (mapcat #(reactivate-tx % opts)) rels)))))))

(defn pose-from-email!
  "Pose a relation triggered by `email` (a pull/entity with :db/id,
  :email/author-address, :email/date-sent).  Sets :setter, :email-eid,
  :posed-at from the email; :from-eid/:to-eid/:kind come from `opts`.
  Caller must have validated via `valid-pose?`."
  [conn email opts]
  (pose-if-absent! conn
                   (merge {:setter    (:email/author-address email)
                           :email-eid (:db/id email)
                           :posed-at  (or (:email/date-sent email) (java.util.Date.))}
                          opts)))

(defn retract-pair!
  "Retract a (from, kind, to) relation if active.  Symmetric kinds
  accept either direction.  Returns true when anything was retracted."
  [conn from-eid kind to-eid retracted-by-email-eid]
  (let [db     (d/db conn)
        active (filterv #(:rel/active? (d/pull db [:rel/active?] %))
                        (keep #(d/entid db [:rel/id %])
                              (paired-relation-ids kind from-eid to-eid)))]
    (when (seq active)
      (d/transact! conn (into [] (mapcat #(retract-tx % retracted-by-email-eid))
                              active))
      true)))

(defn active-inverse-relation
  "Eid of an active relation of `kind` posed in the reverse direction
  (:rel/from = to-eid, :rel/to = from-eid), or nil.  Used to enforce
  last-write-wins on conflicting closure-relation directives."
  [db from-eid to-eid kind]
  (d/q '[:find ?e .
         :in $ ?new-from ?new-to ?kind
         :where
         [?e :rel/from ?new-to]
         [?e :rel/to ?new-from]
         [?e :rel/kind ?kind]
         [?e :rel/active? true]]
       db from-eid to-eid kind))

(defn retract-by-from!
  "Retract every active relation of `kind` with :rel/from = `from-eid`,
  plus its inverse-direction sibling.  Asymmetric kinds only: a
  symmetric kind (:related-to) is canonicalized by ascending eid
  order, so `from-eid` may be stored in :rel/to and a one-sided scan
  would miss half the relations -- use `retract-pair!` for those."
  [conn from-eid kind retracted-by-email-eid]
  {:pre [(contains? asymmetric-kinds kind)]}
  (let [db   (d/db conn)
        eids (d/q '[:find [?e ...]
                    :in $ ?from ?kind
                    :where
                    [?e :rel/from ?from]
                    [?e :rel/kind ?kind]
                    [?e :rel/active? true]]
                  db from-eid kind)]
    (when (seq eids)
      (let [inv-eids (d/q '[:find [?e ...]
                            :in $ ?to ?inv-kind
                            :where
                            [?e :rel/to ?to]
                            [?e :rel/kind ?inv-kind]
                            [?e :rel/active? true]]
                          db from-eid (inverse-kinds kind))]
        ;; Direct datoms, then their inverse-direction siblings.
        (d/transact! conn (into [] (mapcat #(retract-tx % retracted-by-email-eid))
                                (concat eids inv-eids)))
        (count eids)))))

;; ---------------------------------------------------------------------------
;; Patch-closure propagation helpers
;; ---------------------------------------------------------------------------

(defn auto-credit?
  "True iff `bug-eid`.`attr` was set by a labelled patch report that
  :resolves the bug (i.e. via the implicit hook on a \"[PATCH]\" reply).
  Labelless credits (\"Re: [BUG]\" + diff) return false -- no :resolves
  exists, so R3/R4 don't retract them.  `attr` is :report/acked or
  :report/owned."
  [db bug-eid attr]
  (when-let [pose-eid (some-> (d/pull db [{attr [:db/id]}] bug-eid)
                              (get attr)
                              :db/id)]
    (boolean
     (d/q '[:find ?p . :in $ ?bug ?pose
            :where
            [?r :rel/from ?bug]
            [?r :rel/kind :resolved-by]
            [?r :rel/active? true]
            [?r :rel/to ?p]
            [?p :report/email ?pose]]
          db bug-eid pose-eid))))

(defn active-targets
  "Vector of `to`-eids for active outgoing relations of `kind` from `from-eid`."
  [db from-eid kind]
  (d/q '[:find [?to ...] :in $ ?from ?kind
         :where
         [?r :rel/from ?from]
         [?r :rel/kind ?kind]
         [?r :rel/active? true]
         [?r :rel/to ?to]]
       db from-eid kind))

(defn- retract-auto-credit-tx
  [db bug-eid attr addr-attr]
  (let [pulled (d/pull db [{attr [:db/id]} addr-attr] bug-eid)
        pose   (get pulled attr)
        addr   (get pulled addr-attr)]
    (cond-> []
      pose (conj [:db/retract bug-eid attr (:db/id pose)])
      addr (conj [:db/retract bug-eid addr-attr addr]))))

(defn- open-patch-thread-ancestors
  "Open :patch reports counting `patch-eid`'s root email among their
  descendants -- the stale revisions upthread of a patch posted as a
  reply.  Cross-sender by construction, unlike the arrival-time
  auto-supersession which only reaches the same sender's patches.
  Ancestors sharing the patch's series are excluded: applying one
  member must not retire the series' cover letter."
  [db patch-eid]
  (when-let [root-email (:db/id (:report/email
                                 (d/pull db [{:report/email [:db/id]}]
                                         patch-eid)))]
    (d/q '[:find [?r ...]
           :in $ ?e ?self
           :where
           [?r :report/descendants ?e]
           [?r :report/type :patch]
           [(not= ?r ?self)]
           (not [?r :report/closed _])
           (not [?r :report/series ?s]
                [?self :report/series ?s])]
         db root-email patch-eid)))

(defn propagate-patch-closure!
  "Propagate a patch's closure to the bugs/requests it :resolves:
  :resolved closes them; :canceled retracts auto-credits; :superseded
  transfers :owned to `successor-eid` (acked stays with the original
  acker -- a historical act, not transferable).  A :resolved closure
  also supersedes the still-open patch reports upthread: applying the
  head of a revision chain retires the revisions it replaced.  Bumps
  every modified target so the incremental export and notifications
  pick them up."
  [conn patch-eid email-eid close-reason successor-eid]
  (let [db   (d/db conn)
        bugs (active-targets db patch-eid :resolves)]
    (case close-reason
      :resolved
      ;; One snapshot is safe: bug-eids are distinct, so closing
      ;; bug-A doesn't change bug-B's :report/closed.
      (let [closed-bugs (when (seq bugs)
                          (set (d/q '[:find [?b ...]
                                      :in $ [?b ...]
                                      :where [?b :report/closed _]]
                                    db bugs)))
            to-close    (remove #(contains? closed-bugs %) bugs)]
        (doseq [bug-eid to-close]
          (d/transact! conn [{:db/id bug-eid
                              :report/closed email-eid
                              :report/close-reason :resolved}]))
        (when (seq to-close)
          (tracking/bump-report-updated! conn to-close))
        ;; Applying this patch retires its stale upthread revisions:
        ;; close them as :superseded, pose the audit relations, and
        ;; transfer their auto-credits, exactly like an arrival-time
        ;; auto-supersession would have.
        (let [ancestors (open-patch-thread-ancestors db patch-eid)
              closer    (when (seq ancestors)
                          (d/pull db [:db/id :email/author-address
                                      :email/date-sent]
                                  email-eid))]
          (doseq [anc ancestors]
            (d/transact! conn [{:db/id anc
                                :report/closed email-eid
                                :report/close-reason :superseded}])
            (pose-from-email! conn closer {:from-eid anc :to-eid patch-eid
                                           :kind :supersedes})
            (pose-from-email! conn closer {:from-eid anc :to-eid patch-eid
                                           :kind :related-to})
            (propagate-patch-closure! conn anc email-eid
                                    :superseded patch-eid)
            (log/info "Auto-closed patch"
                      (pr-str (:report/message-id
                               (d/pull (d/db conn) [:report/message-id] anc)))
                      "(superseded by applied downthread revision)"))
          (when (seq ancestors)
            (tracking/bump-report-updated! conn ancestors))))

      :canceled
      (let [touched (reduce
                     (fn [acc bug-eid]
                       (let [db' (d/db conn)
                             tx  (cond-> []
                                   (auto-credit? db' bug-eid :report/acked)
                                   (into (retract-auto-credit-tx db' bug-eid
                                                                 :report/acked :report/acked-address))
                                   (auto-credit? db' bug-eid :report/owned)
                                   (into (retract-auto-credit-tx db' bug-eid
                                                                 :report/owned :report/owned-address)))]
                         (if (seq tx)
                           (do (d/transact! conn tx)
                               (conj acc bug-eid))
                           acc)))
                     [] bugs)]
        (when (seq touched)
          (tracking/bump-report-updated! conn touched)))

      :superseded
      ;; Transfer :owned to the successor; :acked is a historical act
      ;; -- whoever first confirmed the bug remains its acker, even
      ;; when the resolving patch is replaced.
      (when successor-eid
        (let [succ (d/pull db [{:report/email [:db/id :email/author-address
                                               :email/date-sent]}]
                           successor-eid)
              succ-eml-eid (some-> succ :report/email :db/id)
              succ-addr    (some-> succ :report/email :email/author-address)
              succ-date    (or (some-> succ :report/email :email/date-sent)
                               (java.util.Date.))
              touched (reduce
                       (fn [acc bug-eid]
                         (let [db' (d/db conn)
                               tx  (when (auto-credit? db' bug-eid :report/owned)
                                     [{:db/id bug-eid
                                       :report/owned succ-eml-eid
                                       :report/owned-address
                                       (some-> succ-addr str/lower-case)}])
                               _   (when (seq tx)
                                     (d/transact! conn tx))
                               ;; Successor inherits the :resolves link
                               ;; (truthy iff something changed).
                               posed (pose-if-absent!
                                      conn {:from-eid successor-eid :to-eid bug-eid
                                            :kind :resolves
                                            :setter succ-addr :email-eid succ-eml-eid
                                            :posed-at succ-date})]
                           (if (or (seq tx) posed)
                             (conj acc bug-eid)
                             acc)))
                       [] bugs)]
          (when (seq touched)
            (tracking/bump-report-updated! conn touched))))

      ;; Other reasons (:expired, etc.) do not propagate.
      nil)))
