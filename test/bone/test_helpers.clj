(ns bone.test-helpers
  "Shared test utilities for Datalevin-backed tests."
  (:require [clojure.java.io :as io]
            [datalevin.core :as d]))

(defn teardown!
  "Close the Datalevin connection and delete the temporary database directory."
  [{:keys [conn db-path]}]
  (d/close conn)
  (let [dir (io/file db-path)]
    (when (.exists dir)
      (doseq [f (reverse (file-seq dir))]
        (.delete f)))))
