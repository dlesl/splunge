(ns splunge.main
  (:require [clojure.pprint :refer [print-table]]
            [jsonista.core :as json]
            [splunge.query :as q]
            [splunge.transducers :as xf]
            [clojure.tools.cli :refer [parse-opts]]
            [clojure.java.io :as io]
            [clojure.string :as str])
  (:import (java.time Instant)
           (java.time.format DateTimeParseException))
  (:gen-class))

(set! *warn-on-reflection* true)

(defn query->format [query]
  (->> query
       (map second)
       (reduce (fn [prev cmd]
                 (case cmd
                   "stats" :table
                   "table" :table
                   "join" :json
                   "transaction" :json
                   prev))
               :json)))

(defn run [query records]
  {:records (sequence (xf/compile-commands query records) records)
   :format (query->format query)})

(defn string->record [s]
  (if s
    (try (assoc (json/read-value s) "_raw" s)
         (catch Exception _ {"_raw" s}))
    {}))

(defn ts&string->record [s]
  (let [[ts raw] (str/split s #"\s+" 2)]
    (try (assoc (string->record raw) "_time" (Instant/parse ts))
         (catch DateTimeParseException _
           (string->record s)))))

(defn format-record [r]
  (if-some [raw (get r "_raw")]
    raw
    (json/write-value-as-string r)))

(defn run-cli [{:keys [query json timestamps input]}]
  (let [input-stream (if input
                       (io/reader input)
                       *in*)
        parse-f (if timestamps ts&string->record string->record)
        {:keys [records format]} (run query (map parse-f (line-seq (java.io.BufferedReader. input-stream))))
        force-format (and json :json)]
    (case (or force-format format)
      :json (doseq [r records]
              (println (format-record r)))
      :table (print-table records)))
  (shutdown-agents))

(def cli-options
  [["-j" "--json" "Always use json output for tables and stats"]
   ["-t" "--timestamps" (str "Lines are prefixed with an ISO8601 timestamp, "
                             "as output by eg. `docker logs -t`")]])

(defn usage [options-summary]
  (->> ["Splunge: searches logs."
        ""
        "Usage: splunge [options] query [input]"
        ""
        "Options:"
        options-summary
        ""]
       (str/join \newline)))

(defn error-msg [errors]
  (str "The following errors occurred while parsing your command:\n\n"
       (str/join \newline errors)))

(defn validate-args [args]
  (let [{:keys [options arguments errors summary]} (parse-opts args cli-options)]
    (cond
      (:help options)
      {:exit-message (usage summary) :ok? true}

      errors
      {:exit-message (error-msg errors)}

      (nil? (first arguments))
      {:exit-message (usage summary)}

      :else
      (let [[query input] arguments]
        (try (let [query (q/parse query)]
               {:options (assoc options
                                :query query
                                :input input)
                :ok? true})
             (catch Exception e
               {:exit-message (error-msg [(.getMessage e)])}))))))

(defn exit [status msg]
  (println msg)
  (System/exit status))

(defn -main [& args]
  (let [{:keys [options exit-message ok?]} (validate-args args)]
    (if exit-message
      (exit (if ok? 0 1) exit-message)
      (try (run-cli options)
           (catch Exception e
             (exit 1 (str e)))))))
