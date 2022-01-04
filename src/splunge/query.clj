(ns splunge.query
  (:require [instaparse.core :as insta :refer [defparser]]
            [instaparse.failure :as failure]
            [clojure.java.io :as io])
  (:import (java.time Duration)))

(set! *warn-on-reflection* true)

(defparser path-parser
  (slurp (io/resource "path.bnf")))

(defn throw-error [parser]
  (fn [& args]
    (let [res (apply parser args)]
      (if (insta/failure? res)
        (throw (ex-info (with-out-str (failure/pprint-failure res))
                        {:type :query}))
        res))))

(defn ->path [s]
  [:path (into [] (insta/transform {:number parse-long} ((throw-error path-parser) s)))])

(defparser query-parser
  (slurp (io/resource "query.bnf")))

(defn parse [s]
  (insta/transform {:field ->path
                    :eval-rhs-field ->path
                    :number parse-long
                    :duration (fn [n unit]
                                (case unit
                                  "ms" (Duration/ofMillis n)
                                  "s" (Duration/ofSeconds n)
                                  "m" (Duration/ofMinutes n)
                                  "h" (Duration/ofHours n)
                                  "d" (Duration/ofDays n)))
                    :boolean #(case %
                                "true" true
                                "false" false)}
                   ((throw-error query-parser) s)))
