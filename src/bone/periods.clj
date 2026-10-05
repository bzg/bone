;; Copyright (c) 2026 Bastien Guerry <bzg@gnu.org>
;; SPDX-License-Identifier: EPL-2.0
;; License-Filename: LICENSES/EPL-2.0.txt

(ns bone.periods
  "Per-source time-windowed overrides.  A source's :periods vector
  declares how :maintainers / :commands / :command-syntax / :labels
  / :restricted-types / :patch-triggers? change over time.  Periods are
  contiguous half-open [from, to) windows; unspecified fields inherit
  source-level defaults.  Pure."
  (:require [bone.common :as common]))

(def ^:private overridable-keys
  #{:maintainers :commands :command-syntax :labels
    :restricted-types :patch-triggers?})

(defn- iso-date? [s]
  ;; parse-iso-date validates the ISO shape (and the calendar) itself.
  (boolean (and (string? s) (common/parse-iso-date s))))

(defn- normalize-period
  "{:from Date|nil :to Date|nil + overridable keys} resolved against
  `source-defaults`."
  [period source-defaults]
  (merge (select-keys source-defaults overridable-keys)
         (select-keys period overridable-keys)
         {:from (common/parse-iso-date (:start period))
          :to   (common/parse-iso-date (:end period))}))

(defn source-periods
  "Chronological normalized periods for `source`.  Without :periods,
  returns a single all-time period from source-level fields."
  [source]
  (if-let [periods (:periods source)]
    (mapv #(normalize-period % source) periods)
    [(normalize-period {} source)]))

(defn period-at-date
  "First period whose [from, to) contains `date` (nil bounds = unbounded)."
  [periods ^java.util.Date date]
  (when date
    (some (fn [{:keys [^java.util.Date from ^java.util.Date to] :as p}]
            (when (and (or (nil? from) (not (.before date from)))
                       (or (nil? to)   (.before date to)))
              p))
          periods)))

(defn source-cfg-at-date
  "Effective source-cfg at `date`: source-level + period overrides.
  Mails dated outside any declared period (in a gap, or past the
  last period's :end) fall back to the source-level config without
  any period override -- an expired period must not bleed past its
  :to.  Without :periods, source-periods produces a single all-time
  period covering every date."
  [source ^java.util.Date date]
  (let [active (period-at-date (source-periods source) date)]
    (cond-> source
      active (merge (select-keys active overridable-keys)))))

(defn- validate-entry [idx period]
  (if-not (map? period)
    [(str "period " idx ": not a map -- " (pr-str period))]
    (cond-> []
      (and (:start period) (not (iso-date? (:start period))))
      (conj (str "period " idx ": :start not ISO yyyy-MM-dd -- "
                 (pr-str (:start period))))
      (and (:end period) (not (iso-date? (:end period))))
      (conj (str "period " idx ": :end not ISO yyyy-MM-dd -- "
                 (pr-str (:end period))))
      (and (iso-date? (:start period)) (iso-date? (:end period))
           (not (neg? (compare (:start period) (:end period)))))
      (conj (str "period " idx ": :start must be strictly before :end")))))

(defn- validate-contiguity [periods]
  (->> (partition 2 1 (map-indexed vector periods))
       (reduce (fn [errs [[ai a] [bi b]]]
                 (cond
                   (nil? (:end a))
                   (conj errs (str "period " ai ": missing :end "
                                   "(only the last period may omit it)"))
                   (nil? (:start b))
                   (conj errs (str "period " bi ": missing :start "
                                   "(only the first period may omit it)"))
                   (not= (:end a) (:start b))
                   (conj errs (str "gap/overlap between periods " ai
                                   " and " bi ": :end " (:end a)
                                   " != :start " (:start b)))
                   :else errs))
               [])))

(defn validate-periods
  "Vector of error strings for `source`'s :periods ([] = valid or absent)."
  [source]
  (let [periods (:periods source)]
    (into (vec (mapcat validate-entry (range) periods))
          (validate-contiguity periods))))
