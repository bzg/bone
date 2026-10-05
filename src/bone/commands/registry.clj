;; Copyright (c) 2026 Bastien Guerry <bzg@gnu.org>
;; SPDX-License-Identifier: EPL-2.0
;; License-Filename: LICENSES/EPL-2.0.txt

(ns bone.commands.registry
  "Pure command registry -- static data, no runtime deps (loadable from bb)."
  (:require [clojure.string :as str]))

;; ---------------------------------------------------------------------------
;; Vocabulary
;;
;;   :kind :trigger    -- state change; acts only on existing reports,
;;                        never on the carrying mail (Closed., Acked.,
;;                        Owned-by:, Superseded-by:, Duplicate-of:).
;;   :kind :annotation -- property set; applies to the report the mail
;;                        creates, else to the nearest in-thread report
;;                        (Urgent., Topic:, Supersedes:, Related-to:).
;;
;; Form is orthogonal: bareword (`:words`) or colon line (`:syntax` +
;; optional `:param`).
;;
;; :scope: :user (anyone -- the default everywhere: updates are
;; auditable and reversible), :maintainer, :setter-or-maintainer (the
;; address that set the attribute, plus any maintainer; only for unset
;; commands with a recoverable setter).  No default entry uses the last
;; two anymore; sources can tighten per command via config.edn
;; `:commands {<cmd-id> {:scope ...}}`.  Role controls (Add
;; maintainer:) are gated separately, in bone.roles.
;; ---------------------------------------------------------------------------

(def commands
  [;; --- Triggers: state-change ----------------------------------------------
   ;; Bareword (sender = setter)
   {:id :acked    :kind :trigger :action :set :attr :report/acked  :scope :user
    :words :acked  :report-types #{:bug :patch :request}}
   {:id :owned    :kind :trigger :action :set :attr :report/owned  :scope :user
    :words :owned  :report-types #{:bug :patch :request}}
   {:id :closed   :kind :trigger :action :set :attr :report/closed :scope :user :words :closed}
   ;; -by lines (credit the update to a third party)
   ;; Reviewed-by is a syntax synonym: acked is BONE's strong approval
   ;; (Confirmed, Approved, Reviewed-by:).  A Reviewed-by both acks and
   ;; is collected as a trailer (:report/trailers) -- even when a
   ;; :scope override denies the ack.
   {:id :acked-by  :kind :trigger :action :set :attr :report/acked  :scope :user
    :syntax "Acked-by" :syntaxes ["Acked-by" "Reviewed-by"]
    :param :email-address :report-types #{:bug :patch :request}}
   {:id :owned-by  :kind :trigger :action :set :attr :report/owned  :scope :user
    :syntax "Owned-by"  :param :email-address :report-types #{:bug :patch :request}}
   {:id :closed-by :kind :trigger :action :set :attr :report/closed :scope :user
    :syntax "Closed-by" :param :email-address}
   ;; Unset lines.  :not-words gives every word of the state's
   ;; resolved vocabulary its "Not <word>" negation; :syntax stays the
   ;; canonical display form.
   {:id :unacked  :kind :trigger :action :unset :attr :report/acked  :scope :user
    :syntax "Not acked"  :not-words :acked :report-types #{:bug :patch :request}}
   {:id :unowned  :kind :trigger :action :unset :attr :report/owned  :scope :user
    :syntax "Not owned"  :not-words :owned :report-types #{:bug :patch :request}}
   ;; No :not-words here: in loose mode a prose "Not fixed." would
   ;; reopen reports.  "Not closed" is the only reopening form.
   {:id :unclosed :kind :trigger :action :unset :attr :report/closed :scope :user
    :syntax "Not closed"}
   ;; Closure relations -- backed by :rel/supersedes / :rel/duplicates;
   ;; :attr kept for registry shape.  Unsets use a direction-suffixed
   ;; :attr so `scope-permits?` finds the right setter on a chained
   ;; report (both directions share the :rel/supersedes schema kind).
   {:id :superseded-by    :kind :trigger :action :set-superseded   :attr :rel/supersedes      :scope :user
    :syntax "Superseded-by"     :param :message-id :report-types #{:bug :patch :request}}
   {:id :unsuperseded-by  :kind :trigger :action :unset-superseded :attr :rel/supersedes-from :scope :user
    :syntax "Not superseded-by" :param :message-id :report-types #{:bug :patch :request}}
   {:id :duplicate-of    :kind :trigger :action :set-duplicate   :attr :rel/duplicates      :scope :user
    :syntax "Duplicate-of"     :param :message-id :report-types #{:bug :patch :request}}
   {:id :unduplicate-of  :kind :trigger :action :unset-duplicate :attr :rel/duplicates-from :scope :user
    :syntax "Not duplicate-of" :param :message-id :report-types #{:bug :patch :request}}

   ;; --- Annotations: property-set -------------------------------------------
   ;; Bareword
   {:id :urgent    :kind :annotation :action :set :attr :report/urgent    :scope :user :words :urgent}
   {:id :important :kind :annotation :action :set :attr :report/important :scope :user :words :important}
   ;; Unset lines for bareword annotations
   {:id :unurgent    :kind :annotation :action :unset :attr :report/urgent    :scope :user
    :syntax "Not urgent" :not-words :urgent}
   {:id :unimportant :kind :annotation :action :unset :attr :report/important :scope :user
    :syntax "Not important" :not-words :important}
   ;; Deadline / expiry / topic
   {:id :deadline    :kind :annotation :action :set-deadline   :attr :report/deadline :scope :user
    :syntax "Deadline" :param :date-or-duration}
   {:id :undeadline  :kind :annotation :action :unset-deadline :attr :report/deadline :scope :user
    :syntax "No deadline"}
   {:id :expiry      :kind :annotation :action :set-expiry   :attr :report/expiry :scope :user
    :syntax "Expiry" :param :date-or-duration}
   {:id :unexpiry    :kind :annotation :action :unset-expiry :attr :report/expiry :scope :user
    :syntax "No expiry"}
   {:id :topic       :kind :annotation :action :set-topic   :attr :report/topic :scope :user
    :syntax "Topic" :param :word}
   {:id :untopic     :kind :annotation :action :unset-topic :attr :report/topic :scope :user
    :syntax "No topic"}
   ;; Supersedes -- inverse role of Superseded-by (posed on the replacement;
   ;; current = :rel/to, target = :rel/from = the closed report).
   {:id :supersedes   :kind :annotation :action :set-supersedes   :attr :rel/supersedes    :scope :user
    :syntax "Supersedes"     :param :message-id :report-types #{:bug :patch :request}}
   {:id :unsupersedes :kind :annotation :action :unset-supersedes :attr :rel/supersedes-to :scope :user
    :syntax "Not supersedes" :param :message-id :report-types #{:bug :patch :request}}
   ;; Related-to -- neutral cross-reference (no closure, multi-target,
   ;; symmetric canonicalised by :rel/id).  Multi-target makes a clean
   ;; setter check impractical, so :unrelated-to stays at :user.
   {:id :related-to   :kind :annotation :action :set-related   :attr :rel/related-to :scope :user
    :syntax "Related-to"     :param :message-id}
   {:id :unrelated-to :kind :annotation :action :unset-related :attr :rel/related-to :scope :user
    :syntax "Not related-to" :param :message-id}])

;; ---------------------------------------------------------------------------
;; Derived indexes
;; ---------------------------------------------------------------------------

;; Syntactic groupings (:words vs :syntax presence).  The semantic
;; axis (:kind) stays per-entry metadata: nothing consumes full lists
;; of triggers/annotations today.
(def word-commands (filterv :words  commands))
(def line-commands (filterv :syntax commands))

(def commands-by-id (into {} (map (juxt :id identity)) commands))

;; Attr lookup for bareword commands -- one bareword per attr, so this
;; mapping is unambiguous (a mapping over all commands would collide
;; for attrs that also have a -by line form).
(def attr->word-cmd
  (into {} (map (juxt :attr identity)) word-commands))

;; Explicit command-id => report attr for bareword commands.  Keeps
;; the id/attr coupling declared here instead of re-deriving attr
;; names from command ids at detection time (`match-words`).
(def word-id->attr
  (into {} (map (juxt :id :attr)) word-commands))

;; Cross-reference annotations (Supersedes:, Related-to: and unsets):
;; neutral links, no state change -- :kind :annotation with a :rel/*
;; :attr.  Closure relations are triggers and excluded.  Used by
;; `apply-commands!` to skip these when broadcasting cover-letter
;; commands (N redundant edges otherwise).
(def cross-ref-line-ids
  (into #{}
        (comp (filter #(and (= :annotation (:kind %))
                            (some-> % :attr namespace (= "rel"))))
              (map :id))
        commands))

;; Bareword state attrs and the attr caching the credited address:
;; the sender, or the designated address of a `-by` line (Acked-by:
;; bob@x credits Bob instead of the sender).
(def address-attrs
  {:report/acked     :report/acked-address
   :report/owned     :report/owned-address
   :report/closed    :report/closed-address
   :report/urgent    :report/urgent-address
   :report/important :report/important-address})

(def proxy-state-attrs (vec (keys address-attrs)))

;; Report attrs tracked as refs to the pose-email:
;;   {ref-attr paired-value-attr-or-nil}
;; topic/deadline/expiry hold their datum in `-value`; proxy-state
;; attrs keep the credited address in `address-attrs` instead.
(def setter-ref-attrs
  {:report/acked         nil
   :report/owned         nil
   :report/closed        nil
   :report/urgent        nil
   :report/important     nil
   :report/topic         :report/topic-value
   :report/deadline      :report/deadline-value
   :report/expiry        :report/expiry-value})

;; Commands that support a :setter-or-maintainer scope override:
;; - unset lines keyed on a setter-ref attribute, plus
;; - explicit relation-backed unsets (:unsuperseded-by, :unsupersedes,
;;   :unduplicate-of).
;; All of them default to :scope :user; this set only matters when a
;; source tightens one back via config.
(def setter-scoped-command-ids
  (into #{:unsuperseded-by :unsupersedes :unduplicate-of}
        (comp (filter #(str/starts-with? (name (:action %)) "unset"))
              (filter #(contains? setter-ref-attrs (:attr %)))
              (map :id))
        commands))
