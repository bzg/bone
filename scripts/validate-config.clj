#!/usr/bin/env bb

;; validate-config.clj -- Validate config.edn against spec.
;;
;; Usage:
;;   bb test-config [path]
;;   bb scripts/validate-config.clj [path]
;;
;; Defaults to ./config.edn if no path given.

(ns validate-config
  (:require [clojure.spec.alpha :as s]
            [clojure.string :as str]
            [clojure.java.io :as io]
            [taoensso.timbre :as log]
            [bone.common :as common]
            [bone.commands.registry :as reg]
            [bone.periods :as periods]))

;; ---------------------------------------------------------------------------
;; Specs
;; ---------------------------------------------------------------------------

;; Primitives
(s/def ::non-blank-string (s/and string? (complement str/blank?)))
(s/def ::pos-int (s/and int? pos?))

;; Email address (basic check: contains @)
(s/def ::email (s/and ::non-blank-string #(str/includes? % "@")))

;; Mailbox connection (IMAP or Maildir).  :name is required and
;; unique across :mailboxes -- it keys the per-mailbox watermark and
;; prefixes log lines.  Shared format with :source/name.
(s/def :mailbox/name common/valid-config-name?)
(s/def :mailbox/type #{:imap :maildir})
(s/def :mailbox/host ::non-blank-string)
(s/def :mailbox/port ::pos-int)
(s/def :mailbox/ssl boolean?)
(s/def :mailbox/user ::non-blank-string)
(s/def :mailbox/password ::non-blank-string)
(s/def :mailbox/oauth2-token ::non-blank-string)
;; Empty string is allowed and means "no subfolder, use :path directly"
;; (typically a Maildir whose root holds cur/new/tmp).
(s/def :mailbox/folder (s/or :empty #{""} :name ::non-blank-string))
(s/def :mailbox/path ::non-blank-string)

(s/def :bone/mailbox
  (s/and (s/keys :req-un [:mailbox/name :mailbox/type]
                 :opt-un [:mailbox/host :mailbox/port :mailbox/ssl
                          :mailbox/user :mailbox/password :mailbox/oauth2-token
                          :mailbox/folder :mailbox/path
                          :bone/ingest])
         (fn [m]
           (case (:type m)
             :imap    (and (:host m) (:user m)
                           (or (:password m) (:oauth2-token m)))
             :maildir (:path m)
             false))))

(s/def :bone/mailboxes
  (s/and (s/coll-of :bone/mailbox :kind vector? :min-count 1)
         (fn [mbs] (common/all-distinct? (map :name mbs)))))

;; Source match spec
(s/def :match/list-id
  (s/and ::non-blank-string
         ;; Must be the bare identifier, not the full header with angle brackets
         (complement #(re-find #"[<>]" %))))
(s/def :match/alias ::non-blank-string)
(s/def :match/to ::non-blank-string)

;; Source -- exactly one of :list, :alias, :to
(defn- exactly-one-source-type? [src]
  (= 1 (count (filter some? (map src [:list :alias :to])))))

;; Source
(s/def :source/name common/valid-config-name?)
(s/def :source/list :match/list-id)
(s/def :source/alias :match/alias)
(s/def :source/to :match/to)
(s/def :source/list-archive (s/and ::non-blank-string #(re-find #"^https?://" %)))
(s/def :source/base-url (s/and ::non-blank-string #(re-find #"^https?://" %)))
(s/def :source/archive-format-string (s/and ::non-blank-string #(str/includes? % "%s")))
;; Project links surfaced on the exported pages (all optional).
(s/def :source/website (s/and ::non-blank-string #(re-find #"^https?://" %)))
(s/def :source/contribute-url (s/and ::non-blank-string #(re-find #"^https?://" %)))
(s/def :source/post-address ::email)

;; Per-source export overrides
(s/def ::export-format #{"json" "rss" "org" "html" "stats" "patches" "text" "events"})
(s/def :source/export-formats (s/coll-of ::export-format :kind vector? :min-count 1))
(s/def :bone/export-formats :source/export-formats)

;; Per-source notifications (optional) -- override global notification gate
(s/def :source-notif/enabled boolean?)
(s/def :source/notifications (s/keys :req-un [:source-notif/enabled]))

;; Per-source maintainers (optional) -- plain list of email strings.
;; The first entry is the lead. "Add/Remove maintainer:" directives
;; mutate the live tenure history at runtime; for historical evolution,
;; use :periods (defined further down, after ::commands-map / ::labels).
(s/def :source/maintainers (s/coll-of ::email :kind vector? :min-count 1))

(s/def ::source
  (s/and (s/keys :req-un [:source/name]
                 :opt-un [:source/list :source/alias :source/to
                          :source/list-archive :source/base-url
                          :source/archive-format-string
                          :source/website :source/contribute-url
                          :source/post-address
                          :source/commands :source/labels
                          :source/report-types :source/restricted-types
                          :source/maintainers :source/notifications
                          :source/expiry :source/awaiting-delay
                          :source/export-formats
                          :source/command-syntax :source/patch-triggers?
                          :source/periods])
         exactly-one-source-type?))

(s/def :bone/sources
  (s/and (s/coll-of ::source :kind vector? :min-count 1)
         (fn [srcs] (common/all-distinct? (map :name srcs)))))

;; DB
(s/def :db/path ::non-blank-string)
(s/def :bone/db (s/keys :req-un [:db/path]))

;; Ingest -- :fetch accepts exactly one of three disjoint map shapes.
(s/def :ingest.fetch/limit pos-int?)
(s/def :ingest.fetch/since
  (s/and string? #(re-matches #"\d+[dwmy]" %)))
(s/def :ingest.fetch/start
  (s/and string? #(re-matches #"\d{4}-\d{2}-\d{2}" %)))
(s/def :ingest.fetch/end
  (s/and string? #(re-matches #"\d{4}-\d{2}-\d{2}" %)))
(s/def :ingest/fetch
  (s/or :count  (s/and (s/keys :req-un [:ingest.fetch/limit])
                       #(= #{:limit} (set (keys %))))
        :since  (s/and (s/keys :req-un [:ingest.fetch/since])
                       #(= #{:since} (set (keys %))))
        :window (s/and (s/keys :opt-un [:ingest.fetch/start :ingest.fetch/end])
                       seq
                       #(every? #{:start :end} (keys %))
                       (fn [{:keys [start end]}]
                         (or (nil? start) (nil? end) (neg? (compare start end)))))))
(s/def :ingest/max-size pos-int?)
(s/def :ingest/max-attachment-size pos-int?)
(s/def :bone/ingest (s/keys :opt-un [:ingest/fetch
                                     :ingest/max-size
                                     :ingest/max-attachment-size]))

;; Theme (optional, global only)
(s/def :bone/theme ::non-blank-string)

;; SMTP
(s/def :smtp/host ::non-blank-string)
(s/def :smtp/port ::pos-int)
(s/def :smtp/user ::non-blank-string)
(s/def :smtp/password ::non-blank-string)
(s/def :smtp/from ::email)
(s/def :smtp/reply-to ::email)
(s/def :smtp/tls boolean?)

(s/def :notif/smtp (s/keys :req-un [:smtp/host :smtp/port :smtp/user :smtp/password :smtp/from]
                           :opt-un [:smtp/tls :smtp/reply-to]))
(s/def :notif/enabled boolean?)

;; Admin Bcc: single email or non-empty vector of emails, copied on
;; every outgoing subscriber digest.  Lets the operator audit
;; deliverability without subscribing each admin on each source
;; individually.
(s/def :notif/admin-bcc
  (s/or :one  ::email
        :many (s/coll-of ::email :kind vector? :min-count 1)))

;; Subscriber filters (all optional except :source)
(s/def :sub/source ::non-blank-string)
(s/def :sub/min-priority (s/and int? #(<= 0 % 3)))
(s/def :sub/min-status   (s/and int? #(<= 0 % 7)))
(s/def :sub/subject-match ::non-blank-string)
(s/def :sub/topic ::non-blank-string)
(s/def :notif/subscription
  (s/keys :req-un [:sub/source]
          :opt-un [:sub/min-priority :sub/min-status :sub/subject-match :sub/topic]))
(s/def :notif/subscribers
  (s/map-of ::email
            (s/coll-of :notif/subscription :kind vector? :min-count 1)))

(s/def :bone/notifications (s/keys :req-un [:notif/enabled]
                                   :opt-un [:notif/smtp :notif/subscribers :notif/admin-bcc]))

;; Valid report type keywords -- derived from common/report-type-spec.
(def valid-report-types common/report-type-keywords)

;; Command IDs (for extended :commands format) -- derived from the
;; registry so a new command is automatically accepted here.
(def valid-command-ids
  (into #{} (map :id) reg/commands))

;; The :setter-or-maintainer scope is only valid on the unset-style
;; commands whose target attribute is tracked by a ref to the
;; pose-email.  The authoritative set is derived from the shared
;; `bone.commands.registry`.
(def valid-plain-scopes  #{:user :maintainer})
(def valid-setter-scopes #{:user :maintainer :setter-or-maintainer})

;; Per-source commands (optional).
;; Values are maps with any of :words, :scope, :report-types (at least one).
;; Each word in :words is a bare string. For historical vocabulary
;; evolution, declare multiple :periods entries on the source.
(s/def ::trigger-words (s/coll-of ::non-blank-string :kind vector? :min-count 1))
(s/def ::command-scope valid-setter-scopes)
(s/def ::command-report-types (s/coll-of valid-report-types :kind set? :min-count 1))

(defn valid-command-value?
  "Validate a single command override. `cmd-id` is needed because
  :setter-or-maintainer is only allowed on the commands listed in
  `reg/setter-scoped-command-ids`."
  [cmd-id v]
  (and (map? v)
       (seq v)
       (every? #{:words :scope :report-types} (keys v))
       (if (:words v) (s/valid? ::trigger-words (:words v)) true)
       (if-let [sc (:scope v)]
         (if (contains? reg/setter-scoped-command-ids cmd-id)
           (contains? valid-setter-scopes sc)
           (contains? valid-plain-scopes sc))
         true)
       (if (:report-types v) (s/valid? ::command-report-types (:report-types v)) true)))

(s/def ::commands-map
  (s/and (s/map-of valid-command-ids any?)
         #(every? (fn [[k v]] (valid-command-value? k v)) %)))

(s/def :source/commands ::commands-map)

;; Global commands (optional) -- same shape as per-source
(s/def :bone/commands ::commands-map)

;; Subject triggers: map of report-type keyword -> vector of tag strings
;; e.g. {:bug ["BUG" "DEFECT"] :request ["POLL" "TODO" "FR"]}
(s/def ::label-tags (s/coll-of ::non-blank-string :kind vector? :min-count 1))
(s/def ::labels
  (s/map-of #{:bug :patch :request :announcement :release :change}
            ::label-tags))
(s/def :source/labels ::labels)
(s/def :bone/labels ::labels)

;; Report types: filters which report types are detected at ingest
;; AND exported. Default: all types. Per-source overrides global.
(s/def ::report-types
  (s/coll-of valid-report-types :kind set? :min-count 1))
(s/def :source/report-types ::report-types)
(s/def :bone/report-types ::report-types)

;; Restricted types: which report types require maintainer status to
;; create. Default: #{:announcement :release :change}. The empty set
;; opens every type to any sender. Per-source overrides global.
(s/def ::restricted-types
  (s/coll-of valid-report-types :kind set?))
(s/def :source/restricted-types ::restricted-types)
(s/def :bone/restricted-types ::restricted-types)

;; Expiry rules (optional)
;; Each report type maps to a rule map with :inactive-after and optional conditions.
(s/def :expiry/inactive-after (s/or :deadline #{:deadline}
                                    ;; parse-iso-date checks the shape AND the
                                    ;; calendar: "2026-02-30" would pass a bare
                                    ;; regex, then silently disable the rule.
                                    :date (s/and ::non-blank-string
                                                 #(some? (common/parse-iso-date %)))
                                    ;; Full match, not re-seq: "30 days" must be
                                    ;; rejected here, parse-duration-str throws on it.
                                    :string (s/and ::non-blank-string #(re-matches #"(?:\d+\s*[ydwm]\s*)+" %))
                                    :int pos-int?))
(s/def :expiry/max-status (s/and int? #(<= 0 % 3)))
(s/def :expiry/max-priority (s/and int? #(<= 0 % 3)))

(s/def ::expiry-rule
  (s/keys :req-un [:expiry/inactive-after]
          :opt-un [:expiry/max-status :expiry/max-priority]))

(s/def ::expiry
  (s/map-of valid-report-types ::expiry-rule))
(s/def :source/expiry ::expiry)
(s/def :bone/expiry ::expiry)

;; Logging (optional)
(s/def :logging/file ::non-blank-string)
(s/def :logging/level #{:debug :info :warn :error})
(s/def :logging/max-size (s/and ::non-blank-string #(re-matches #"\d+[KMG]B" (str/upper-case (str/trim %)))))
(s/def :logging/backlog ::pos-int)

;; Logging :email -- sends log entries via :notifications :smtp
(s/def :log-email/to ::email)
(s/def :log-email/level #{:debug :info :warn :error})
(s/def :logging/email (s/keys :req-un [:log-email/to]
                              :opt-un [:log-email/level]))

(s/def :bone/logging (s/keys :opt-un [:logging/file :logging/level :logging/max-size
                                      :logging/backlog :logging/email]))

;; Awaiting-reply delay -- same units as parse-duration-str (d/w/m/y)
(s/def :bone/awaiting-delay
  (s/and ::non-blank-string #(re-matches #"\d+[dwmy](?:\s+\d+[dwmy])*" %)))
(s/def :source/awaiting-delay :bone/awaiting-delay)

;; Command syntax mode: :loose (default -- ! is optional on every Bone
;; instruction) or :strict (! required on every Bone instruction).
;; For historical evolution of this setting, declare multiple periods
;; on the source.
(s/def :bone/command-syntax #{:loose :strict})
(s/def :source/command-syntax :bone/command-syntax)

;; Whether patches on this source act as triggers on the bugs/requests
;; they resolve. When false, a patch in reply to a bug/request does not
;; auto-set Acked/Owned, and closing the patch as :resolved does not
;; close the parent. Default true.
(s/def :bone/patch-triggers? boolean?)
(s/def :source/patch-triggers? :bone/patch-triggers?)
(s/def :period/patch-triggers? :bone/patch-triggers?)

;; Per-source periods (optional) -- time-windowed overrides for
;; :maintainers / :commands / :command-syntax / :labels /
;; :restricted-types / :patch-triggers?.
;; Each period is a map with optional :start, :end (ISO yyyy-MM-dd) and
;; any subset of the overridable keys. Periods must be contiguous.
;; Only the first may omit :start (unbounded past); only the last may
;; omit :end (still active). Defined here, below ::commands-map and
;; ::labels, so spec resolution succeeds under bb.
(s/def :period/start (s/and ::non-blank-string #(re-matches #"\d{4}-\d{2}-\d{2}" %)))
(s/def :period/end :period/start)
(s/def :period/maintainers :source/maintainers)
(s/def :period/commands ::commands-map)
(s/def :period/command-syntax :bone/command-syntax)
(s/def :period/labels ::labels)
(s/def :period/restricted-types ::restricted-types)
(s/def ::period-entry
  (s/keys :opt-un [:period/start :period/end
                   :period/maintainers :period/commands
                   :period/command-syntax :period/patch-triggers?
                   :period/labels :period/restricted-types]))
(s/def :source/periods (s/coll-of ::period-entry :kind vector? :min-count 1))

;; Top-level config
(s/def ::config
  (s/keys :req-un [:bone/mailboxes :bone/sources]
          :opt-un [:bone/db :bone/ingest :bone/notifications :bone/labels
                   :bone/commands
                   :bone/report-types :bone/restricted-types
                   :bone/awaiting-delay :bone/patch-triggers?
                   :bone/expiry :bone/logging
                   :bone/command-syntax :bone/theme
                   :bone/export-formats]))

;; ---------------------------------------------------------------------------
;; Validation
;; ---------------------------------------------------------------------------

(defn- commands-map-errors
  "Walk a :commands map (top-level or per-source) and return a seq of
  human-readable error strings.  Used to surface precise, actionable
  messages before falling back to `s/explain-str`."
  [where commands-map]
  (when (map? commands-map)
    (for [[cmd-id v] commands-map
          :let [errs (cond
                       (not (contains? valid-command-ids cmd-id))
                       [(str "unknown command id " (pr-str cmd-id))]

                       (not (map? v))
                       [(str "expected a map with any of :words, :scope, "
                             ":report-types, got " (pr-str v))]

                       (empty? v)
                       [(str "expected at least one of :words, :scope, "
                             ":report-types")]

                       :else
                       (let [bad-keys   (remove #{:words :scope :report-types} (keys v))
                             sc         (:scope v)
                             allows-s-o-m? (contains? reg/setter-scoped-command-ids cmd-id)
                             allowed-scopes (if allows-s-o-m?
                                              valid-setter-scopes
                                              valid-plain-scopes)]
                         (concat
                          (when (seq bad-keys)
                            [(str "unknown key(s): "
                                  (str/join ", " (map pr-str bad-keys)))])
                          (when (and sc (not (contains? allowed-scopes sc)))
                            [(str ":scope " (pr-str sc)
                                  " is not valid for " (pr-str cmd-id)
                                  ". Valid values: "
                                  (str/join ", "
                                            (map pr-str (sort allowed-scopes))))]))))]
          err errs]
      (str where " " (pr-str cmd-id) ": " err))))

(defn- pre-check-commands
  "Return a seq of human-readable errors for the top-level :commands
  map and each source's :commands map, or nil.  A top-level
  :global-commands is rejected: the application only reads :commands,
  so that key would be silently ignored at runtime."
  [config]
  (let [errs (concat (when (contains? config :global-commands)
                       [(str ":global-commands is not a top-level config key"
                             " -- use :commands for global defaults")])
                     (commands-map-errors ":commands" (:commands config))
                     (mapcat (fn [src]
                               (commands-map-errors
                                (str ":sources [" (pr-str (:name src)) "] :commands")
                                (:commands src)))
                             (:sources config)))]
    (seq errs)))

(defn- pre-check-periods
  "Validate :periods on each source via bone.periods/validate-periods."
  [config]
  (seq
   (mapcat (fn [src]
             (map (fn [err]
                    (str ":sources [" (pr-str (:name src)) "] " err))
                  (periods/validate-periods src)))
           (:sources config))))

(defn- pre-check-subscribers
  "Verify every :source named under :notifications :subscribers
  matches an existing source :name.  Skips if the shape is wrong
  (spec validation reports the structural error)."
  [config]
  (when-let [subs (get-in config [:notifications :subscribers])]
    (when (map? subs)
      (let [known (set (map :name (:sources config)))
            errs  (for [[email subscriptions] subs
                        :when (sequential? subscriptions)
                        {:keys [source]} subscriptions
                        :when (and source (not (contains? known source)))]
                    (str ":notifications :subscribers [" (pr-str email)
                         "] :source " (pr-str source)
                         " does not match any :sources :name"))]
        (seq errs)))))

(defn- pre-check-singleton-mailbox
  "BONE is in 0.y.z -- reject the singleton :mailbox key outright
  (no silent wrap to :mailboxes)."
  [config]
  (when (contains? config :mailbox)
    [common/singleton-mailbox-error]))

(defn validate-config [config]
  ;; concat, not or: show every pre-check category at once instead of
  ;; making the user fix one family of errors per run.
  (if-let [errs (seq (concat (pre-check-singleton-mailbox config)
                             (pre-check-commands config)
                             (pre-check-periods config)
                             (pre-check-subscribers config)))]
    {:valid? false
     :explanation (str/join "\n" errs)}
    (if (s/valid? ::config config)
      (cond-> {:valid? true}
        (and (get-in config [:logging :email])
             (not (get-in config [:notifications :smtp])))
        (assoc :warnings ["Logging :email is configured but :notifications :smtp is absent."]))
      {:valid? false
       :explanation (s/explain-str ::config config)})))

;; ---------------------------------------------------------------------------
;; Main
;; ---------------------------------------------------------------------------

(let [path (or (first *command-line-args*)
               (System/getenv "BONE_CONFIG")
               "config.edn")
      file (io/file path)]
  (if-not (.exists file)
    (do (log/error "Config file not found:" path)
        (System/exit 1))
    (let [config (try
                   (common/load-config path)
                   (catch Exception e
                     (log/error "Invalid EDN:" (.getMessage e))
                     (System/exit 1)))
          result (validate-config config)
          inline-pwds (common/inline-password-locations path)]
      (doseq [loc inline-pwds]
        (log/warn "Inline :password in" loc
                  "-- consider :password-file or #bone/env to keep"
                  "credentials out of config.edn (see manual)."))
      (if (:valid? result)
        (do (log/info "✓" path "is valid.")
            (log/info "  Mailboxes:" (count (:mailboxes config)))
            (doseq [mb (:mailboxes config)]
              (log/info "    -" (:name mb) (pr-str (:type mb))
                        (case (:type mb)
                          :imap    (str (:user mb) "@" (:host mb) "/" (or (:folder mb) "INBOX"))
                          ;; No default subfolder for a maildir: the path
                          ;; itself is the box (:folder only if declared).
                          :maildir (cond-> (str/replace (common/expand-home (:path mb)) #"/+$" "")
                                     (:folder mb) (str "/" (:folder mb)))
                          ""))
              (when-let [ing (:ingest mb)]
                (log/info "        :ingest" (pr-str ing))))
            (log/info "  Sources:" (count (:sources config)))
            (doseq [src (:sources config)]
              (let [parts (cond-> []
                            (:list src)          (conj (str "(list: " (:list src) ")"))
                            (:alias src)         (conj (str "(alias: " (:alias src) ")"))
                            (:to src)            (conj (str "(mailbox: " (:to src) ")"))
                            (:list-archive src)  (conj (str "archive: " (:list-archive src)))
                            (:report-types src)  (conj (str "report-types: " (pr-str (:report-types src))))
                            (:command-syntax src) (conj (str "command-syntax: "
                                                              (name (:command-syntax src))))
                            (false? (:patch-triggers? src))
                            (conj "patch-triggers: off")
                            (seq (:maintainers src))
                            (conj (str "maintainers: "
                                       (str/join ", " (:maintainers src))))
                            (seq (:periods src))
                            (conj (str "periods: " (count (:periods src))))
                            (some? (get-in src [:notifications :enabled]))
                            (conj (str "notify: " (get-in src [:notifications :enabled]))))]
                (log/info "    -" (:name src) (str/join " " parts))))
            (log/info "  DB path:" (or (common/expand-home (get-in config [:db :path]))
                                        "data/bone-db (default)"))
            (when-let [ingest (:ingest config)]
              (when-let [v (:fetch ingest)]
                (log/info "  Fetch:" (pr-str v))))
            (when-let [notif (:notifications config)]
              (log/info "  Notifications:" (if (:enabled notif) "enabled" "disabled"))
              (when-let [smtp (:smtp notif)]
                (log/info "  SMTP:" (str (:user smtp) "@" (:host smtp))))
              (when-let [subs (:subscribers notif)]
                (let [total (reduce + (map count (vals subs)))]
                  (log/info "  Subscribers:" (count subs) "email(s),"
                            total "subscription(s)"))))
            (when-let [rt (:report-types config)]
              (log/info "  Report types:" (pr-str rt)))
            (when-let [cs (:command-syntax config)]
              (log/info "  Command syntax (global):" (name cs)))
            (when-let [logging (:logging config)]
              (when (:file logging)
                (log/info "  Log file:" (:file logging)
                          "level:" (or (:level logging) :warn)))
              (when-let [em (:email logging)]
                (log/info "  Log email:" (:to em)
                          "level:" (or (:level em) :error))))
            (doseq [w (:warnings result)]
              (log/warn "⚠" w)))
        (do (log/error "✗" path "is invalid:")
            (log/error (:explanation result))
            (System/exit 1))))))
