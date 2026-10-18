;; Copyright (c) 2026 Bastien Guerry <bzg@gnu.org>
;; SPDX-License-Identifier: EPL-2.0
;; License-Filename: LICENSES/EPL-2.0.txt

(ns bone.ics
  "iCalendar (RFC 5545) helpers: attachment and inline detection,
  VEVENT/VTIMEZONE extraction and deduplication, text escaping, line
  folding and VCALENDAR assembly.  Pure, shared JVM/bb."
  (:require [clojure.string :as str]))

;; ---------------------------------------------------------------------------
;; Detection
;; ---------------------------------------------------------------------------

(defn ics-file?
  "True if filename looks like an ICS calendar file."
  [filename]
  (boolean (and filename (re-find #"(?i)\.ics$" filename))))

(defn has-ics-attachment?
  "True if any attachment has a .ics filename."
  [attachments]
  (boolean (some #(ics-file? (:attachment/filename %)) attachments)))

(defn has-inline-ics?
  "True if body text contains inline ICS/VCALENDAR content."
  [body-text]
  (and (some? body-text)
       (str/includes? body-text "BEGIN:VCALENDAR")
       (str/includes? body-text "BEGIN:VEVENT")))

;; ---------------------------------------------------------------------------
;; ICS (iCalendar) parsing and assembly -- pure, shared JVM/bb.
;; ---------------------------------------------------------------------------

(defn normalize-ics-eol
  "Normalize line endings to CRLF as required by RFC 5545.  Lone LF, lone
  CR, and existing CRLF all collapse to a single CRLF.  nil-safe."
  [s]
  (when s (str/replace s #"\r\n?|\n" "\r\n")))

(defn- ics-component-re [component]
  (re-pattern (str "(?s)BEGIN:" component "(?:(?!BEGIN:" component ").)*?END:" component)))

(defn- extract-ics-blocks
  "Extract BEGIN:<component>...END:<component> blocks from ICS text.  Each
  returned block is CRLF-normalized and CRLF-terminated.  The reluctant
  quantifier is bounded by the next BEGIN to avoid catastrophic
  backtracking and to keep successive components from merging."
  [component text]
  (when (and text (str/includes? text (str "BEGIN:" component)))
    (map #(str (normalize-ics-eol %) "\r\n")
         (re-seq (ics-component-re component) text))))

(defn extract-vevents
  "Extract CRLF-normalized VEVENT blocks from ICS text."
  [text]
  (extract-ics-blocks "VEVENT" text))

(defn extract-vtimezones
  "Extract CRLF-normalized VTIMEZONE blocks from ICS text."
  [text]
  (extract-ics-blocks "VTIMEZONE" text))

(defn- unfold-ics
  "Undo RFC 5545 line folding (a CRLF followed by a space or tab)."
  [s]
  (str/replace s #"\r\n[ \t]" ""))

(defn ics-property
  "Read a top-level ICS property value (ignoring any parameters) from a
  component block, or nil.  Case-insensitive on the property name."
  [block prop]
  (when block
    (let [re (re-pattern (str "(?im)^" (java.util.regex.Pattern/quote prop)
                              "(?:;[^:\r\n]*)?:(.*)$"))]
      (some-> (re-find re (unfold-ics block)) second str/trim))))

(defn- vevent-sequence
  "SEQUENCE number of a VEVENT block (0 when absent or unparseable)."
  [block]
  (or (some-> (ics-property block "SEQUENCE") parse-long) 0))

(defn dedupe-vevents
  "Deduplicate VEVENT blocks, preserving first-seen order.  Blocks with a
  UID are correlated by [UID, RECURRENCE-ID], keeping the highest SEQUENCE;
  RECURRENCE-ID is part of the key so per-occurrence overrides of a
  recurring event (same UID, distinct RECURRENCE-ID) survive rather than
  collapsing into the master.  Blocks without a UID cannot be correlated,
  so they are deduplicated only when byte-for-byte identical -- the
  normalized block text itself is the key."
  [vevents]
  (let [vkey (fn [v] (if-let [uid (ics-property v "UID")]
                       [uid (ics-property v "RECURRENCE-ID")]
                       v))
        best (reduce (fn [m v]
                       (let [k (vkey v)]
                         (if (and (m k) (>= (vevent-sequence (m k)) (vevent-sequence v)))
                           m
                           (assoc m k v))))
                     {} vevents)]
    ;; distinct preserves first-seen key order.
    (mapv best (distinct (map vkey vevents)))))

(defn dedupe-vtimezones
  "Keep one VTIMEZONE per TZID, first-seen.  VTIMEZONEs without a TZID are
  dropped -- a TZID is mandatory and an anonymous one is unreferenceable."
  [vtimezones]
  (->> vtimezones
       (filter #(ics-property % "TZID"))
       (reduce (fn [[acc seen] vtz]
                 (let [tzid (ics-property vtz "TZID")]
                   (if (seen tzid)
                     [acc seen]
                     [(conj acc vtz) (conj seen tzid)])))
               [[] #{}])
       first))

(defn escape-ics-text
  "Escape an ICS TEXT-typed property value per RFC 5545 section 3.3.11:
  backslash, semicolon and comma are backslash-escaped, and newlines
  become the literal `\\n`.  nil-safe."
  [s]
  (when s
    (-> s
        (str/replace "\\" "\\\\")
        (str/replace ";" "\\;")
        (str/replace "," "\\,")
        (str/replace #"\r\n?|\n" "\\\\n"))))

(defn fold-ics-line
  "Fold a single content line to <=75 octets per RFC 5545 section 3.1,
  inserting a CRLF followed by one space at each fold point.  Counts
  UTF-8 octets (not chars) and never splits a multi-octet character.
  Returns the folded line without a trailing CRLF.  nil-safe."
  [line]
  (when line
    (let [^String s line
          limit    75
          len      (.length s)]
      (loop [i 0
             width 0
             ^StringBuilder out (StringBuilder.)]
        (if (>= i len)
          (.toString out)
          ;; Iterate by code point, not by UTF-16 char: a non-BMP
          ;; character (e.g. an emoji) is a surrogate pair whose two
          ;; halves must never be separated by a fold, and whose UTF-8
          ;; size (4 octets) must be counted as a whole.
          (let [c  (.charAt s i)
                cp (if (and (Character/isHighSurrogate c)
                            (< (inc i) len)
                            (Character/isLowSurrogate (.charAt s (inc i))))
                     (str c (.charAt s (inc i)))
                     (str c))
                n  (alength (.getBytes cp "UTF-8"))
                i' (+ i (.length ^String cp))]
            (if (> (+ width n) limit)
              (recur i' (+ 1 n) (-> out (.append "\r\n ") (.append cp)))
              (recur i' (+ width n) (.append out cp)))))))))

(defn build-vcalendar
  "Assemble a complete VCALENDAR document (CRLF-terminated) wrapping
  the given VTIMEZONE and VEVENT blocks; nil when no event.  Always
  METHOD:PUBLISH (read-only subscription feed): any iTIP METHOD from
  the source is dropped, but STATUS:CANCELLED inside a block is
  preserved verbatim."
  [cal-name vevents vtimezones]
  (when (seq vevents)
    (str "BEGIN:VCALENDAR\r\n"
         "VERSION:2.0\r\n"
         "PRODID:-//BONE//Event Export//EN\r\n"
         "METHOD:PUBLISH\r\n"
         (fold-ics-line (str "X-WR-CALNAME:" (escape-ics-text cal-name))) "\r\n"
         (str/join "" vtimezones)
         (str/join "" vevents)
         "END:VCALENDAR\r\n")))
