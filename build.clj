(ns build
  (:require [clojure.tools.build.api :as b]
            [org.corfield.build :as bb]))


(defn uber "Run the CI pipeline of tests (and build the JAR)." [opts]
  (-> opts
      (assoc :lib 'splunge :main 'splunge.main)
      (bb/run-tests)
      (bb/clean)
      (bb/uber)))
