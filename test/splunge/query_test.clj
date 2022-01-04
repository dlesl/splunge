(ns splunge.query-test
  (:require [clojure.test :refer :all]
            [splunge.query :refer :all]))

(deftest search
  (is (= [[:command
           "search"
           [:search-expression
            [:or-search-expression
             [:search-expression
              [:comparison-expression [:path ["test"]] [:value "test"]]]
             [:search-expression
              [:bracket-search-expression
               [:search-expression
                [:in-search-expression
                 [:path ["val"]]
                 [:value "one"]
                 [:value "two"]]]]]]]]]
         (parse "search test=test OR (val IN (one two))"))))

(deftest transaction
  (is (= [[:command "transaction" [:path ["test"]] [:transaction-opt "startswith" [:value "test"]]]]
         (parse "transaction test startswith=test"))))
