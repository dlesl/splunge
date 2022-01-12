(ns splunge.main-test
  (:require [clojure.test :refer [deftest is testing]]
            [splunge.main :refer [run string->record]]
            [splunge.query :as q]))

(def input (map string->record ["{\"id\": 1, \"stuff\": 2, \"other\": 5}"
                                "{\"id\": 1, \"stuff\": 4}"
                                "{\"id\": 2, \"stuff\": 3}"]))

(def input2 (map string->record ["{\"first1\": \"one\", \"second1\": \"1\"}"
                                 "{\"first1\": \"one\", \"second1\": \"2\"}"
                                 "{\"first1\": \"one\", \"second1\": \"3\"}"
                                 "{\"first1\": \"one\", \"second1\": \"4\"}"]))

(defn parse&run [query records]
  (run (q/parse query) records))

(deftest sample-queries
  (testing "search"
    (is (= [1 1]
           (->> (parse&run "search id=1" input)
                :records
                (map #(get % "id"))))))

  (testing "join"
    (is (= [5]
           (->> (parse&run "search stuff=4 | join id [search stuff=2]" input)
                :records
                (map #(get % "other"))))))

  (testing "stats"
    (is (= [{"id" 1 "count" 2}
            {"id" 2 "count" 1}]
           (->> (parse&run "stats count by id" input)
                :records
                (map #(dissoc % "_raw")))))

    (is (= [{"id" 1 "stuff" 2 "count" 1}
            {"id" 1 "stuff" 4 "count" 1}
            {"id" 2 "stuff" 3 "count" 1}]
           (->> (parse&run "stats count by id, stuff" input)
                :records
                (map #(dissoc % "_raw")))))

    (is (= [{"count" 3}]
           (->> (parse&run "stats count" input)
                :records
                (map #(dissoc % "_raw")))))

    (is (= [{"count" 4 "avg" 2.5}]
           (->> (parse&run "stats count, avg(second1)" input2)
                :records
                (map #(dissoc % "_raw"))))))

  (testing "table"
    (is (= [{"id" 1}
            {"id" 1}]
           (:records (parse&run "search id=1 | table id" input)))))

  (testing "sort"
    (is (= [{"id" 1}
            {"id" 2}
            {"id" 1}]
           (:records (parse&run "sort stuff | table id" input))))
    (is (= [{"stuff" 4}
            {"stuff" 2}
            {"stuff" 3}]
           (:records (parse&run "sort id, - stuff | table stuff" input))))
    (is (= [{"id" 2}
            {"id" 1}
            {"id" 1}]
           (:records (parse&run "sort - id | table id" input)))))

  (testing "output format"
    (is (= :table (:format (parse&run "stats count by id" []))))
    (is (= :json (:format (parse&run "search ok" [])))))

  (testing "transaction"
    (is (= [1 2]
           (->> (parse&run "transaction id" input)
                :records
                (map #(get % "id")))))
    (is (= [1]
           (->> (parse&run "transaction id startswith=other" input)
                :records
                (map #(get % "id")))))

    (is (= [1]
           (->> (parse&run "transaction id startswith=(other=5)" input)
                :records
                (map #(get % "id")))))
    (is (= ["3"]
           (->> (parse&run "transaction first1 startswith=(second1=1) endswith=(second1=3)" input2)
                :records
                (map #(get % "second1")))))
    (is (= ["3" "4"]
           (->> (parse&run "transaction first1 startswith=(second1=1) endswith=(second1=3) keeporphans=true" input2)
                :records
                (map #(get % "second1")))))
    (is (= []
           (->> (parse&run "transaction first1 startswith=(second1=1) endswith=(second1=3) maxevents=2" input2)
                :records
                (map #(get % "second1")))))
    (is (= ["3"]
           (->> (parse&run "transaction first1 startswith=(second1=1) endswith=(second1=3) maxevents=3" input2)
                :records
                (map #(get % "second1")))))
    (is (= ["1" "2" "3" "4"]
           (->> (parse&run "transaction first1 startswith=(second1=1) endswith=(second1=3) maxevents=2 keeporphans=true" input2)
                :records
                (map #(get % "second1"))))))
  (testing "eval"
    (is (= [5 nil nil]
           (->> (parse&run "eval id=other" input)
                :records
                (map #(get % "id")))))
    (is (= [1 0 0]
           (->> (parse&run "eval id=if(other,1,0)" input)
                :records
                (map #(get % "id")))))
    (is (= [1 0 0]
           (->> (parse&run "eval id=if(other==5,1,0)" input)
                :records
                (map #(get % "id")))))
    (is (= [1 0 0]
           (->> (parse&run "eval id=if(other==\"5\",1,0)" input)
                :records
                (map #(get % "id")))))
    (is (= [5 "not other" "not other"]
           (->> (parse&run "eval id=if(other LIKE \"_\",other,\"not other\")" input)
                :records
                (map #(get % "id")))))
    (is (= [5 4 3]
           (->> (parse&run "eval out=coalesce(other,stuff)" input)
                :records
                (map #(get % "out"))))))

  (testing "fillnull"
    (is (= [5 0 0]
           (->> (parse&run "fillnull other" input)
                :records
                (map #(get % "other")))))
    (is (= [5 "ok" "ok"]
           (->> (parse&run "fillnull value=\"ok\" other" input)
                :records
                (map #(get % "other")))))

    (is (= ["one"]
           (->> (parse&run "dedup first1" input2)
                :records
                (map #(get % "first1")))))
    (is (= ["one" "one"] (->> (parse&run "dedup 2 first1" input2)
                              :records
                              (map #(get % "first1")))))
    (is (= ["1" "2" "3" "4"] (->> (parse&run "dedup first1 second1" input2)
                                  :records
                                  (map #(get % "second1")))))))
