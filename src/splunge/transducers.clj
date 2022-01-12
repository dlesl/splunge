(ns splunge.transducers
  (:refer-clojure :exclude [eval])
  (:require [clojure.string :as str]
            [clojure.core.match :refer [match]]
            [clojure.core.incubator :refer [dissoc-in]]
            [splunge.utils :refer [assoc|dissoc assoc|dissoc-in]]
            [net.cgrand.xforms :as xforms])
  (:import (java.time Instant Duration)))

(set! *warn-on-reflection* true)

(defn- wildcard->pattern [wildcard]
  (->> (str/split wildcard #"[*]" -1)
       (map #(java.util.regex.Pattern/quote %))
       (str/join ".*")
       re-pattern))

(defn- like->pattern [s]
  (->> s
       (map #(case %
               \% ".*"
               \_ "."
               (java.util.regex.Pattern/quote (str %))))
       (apply str)
       re-pattern))

(defn- matches?
  ([r pattern]
   ;; TODO: search all keys and values?
   (some? (re-find pattern (get r "_raw"))))
  ([r path pattern]
   (when-some [value (get-in r path)]
     (some? (re-matches pattern (str value))))))

(defn- search-predicate [arg]
  (match [arg]
    [[:search-expression search-expression]]
    (match [search-expression]
      [[:value v]]
      (let [pat (wildcard->pattern v)]
        #(matches? % pat))

      [[:comparison-expression [:path path] [:value v]]]
      (let [pat (wildcard->pattern v)]
        #(matches? % path pat))

      [[:or-search-expression & exprs]]
      (let [subexprs (map search-predicate exprs)]
        #(some (fn [f] (f %)) subexprs))

      [[:not-search-expression expr]]
      (complement (search-predicate expr))

      [[:in-search-expression [:path path] & values]]
      (let [pats (->> values
                      (map second)
                      (mapv wildcard->pattern))]
        #(some (fn [pat] (matches? % path pat)) pats))

      [[:bracket-search-expression & exprs]]
      (let [subexprs (map search-predicate exprs)]
        #(every? (fn [pred] (pred %)) subexprs)))))

(defn search [exprs]
  (->> exprs
       (map search-predicate)
       (map filter)
       (reduce comp)))

(defn- path->string [path]
  (let [sb (StringBuilder.)]
    (doseq [el path]
      (cond
        (string? el) (doto sb
                       (.append ".")
                       (.append el))
        (integer? el) (doto sb
                        (.append "{")
                        (.append el)
                        (.append "}"))))
    (if (string? (first path))
      (.substring sb 1)
      (str sb))))

(defn _sort [sort-bys]
  (letfn [(cmp [sort-bys]
            (if-some [[sort-by & sort-bys] (seq sort-bys)]
              (let [next (cmp sort-bys)]
                (match [sort-by]
                  [[:sort-by "-" [:path path]]]
                  #(let [res (compare (get-in %2 path) (get-in %1 path))]
                     (if (zero? res)
                       (next %1 %2)
                       res))

                  [(:or [:sort-by "+" [:path path]] [:sort-by [:path path]])]
                  #(let [res (compare (get-in %1 path) (get-in %2 path))]
                     (if (zero? res)
                       (next %1 %2)
                       res))))
              (constantly 0)))]
    (xforms/sort (cmp sort-bys))))

(defn ->number [v]
  (cond
    (number? v) v
    (string? v) (or (parse-long v) (parse-double v))
    (instance? Instant v) (.toEpochMilli ^Instant v)
    :else nil))

(defn stats [fns paths]
  (let [paths (mapv second paths)
        kfn #(mapv (fn [path] (get-in % path)) paths)
        path-names (mapv path->string paths)
        xfs (->> fns
                 (map (fn [f] (match f
                                     [:stats-fn "count"]
                                     {"count" xforms/count}

                                     [:stats-fn number-fn [:path path]]
                                     {number-fn (comp (map #(->number (get-in % path)))
                                                      (case number-fn
                                                        "avg" xforms/avg
                                                        "max" xforms/maximum
                                                        "min" xforms/minimum))})))
                 (apply merge))]
    (comp
     (xforms/by-key kfn (xforms/transjuxt xfs))
     (map (fn [[ks v]]
            (merge (zipmap path-names ks) v))))))

(defn maybe-maths [op left right]
  (when-some [left (->number left)]
    (when-some [right (->number right)]
      (op left right))))

(defn eval-equals? [left right]
  (and (some? left) (some? right)
       (or (= left right)
           (when-some [left (->number left)]
             (when-some [right (->number right)]
               (= left right))))))

(defn eval-fn [expr]
  (match [expr]
    [[:path path]]
    #(get-in % path)

    [[:literal literal]]
    (constantly literal)

    [[:eval-operator left operator right]]
    (let [left-f (eval-fn left)
          right-f (eval-fn right)]
      (case operator
        "." #(str (left-f %) (right-f %))
        "+" #(maybe-maths + (left-f %) (right-f %))
        "-" #(maybe-maths - (left-f %) (right-f %))
        "*" #(maybe-maths * (left-f %) (right-f %))
        "/" #(maybe-maths / (left-f %) (right-f %))
        "%" #(maybe-maths mod (left-f %) (right-f %))
        "=" #(eval-equals? (left-f %) (right-f %))
        "==" #(eval-equals? (left-f %) (right-f %))
        ">" #(maybe-maths > (left-f %) (right-f %))
        "<" #(maybe-maths < (left-f %) (right-f %))))

    [[:eval-like left [:literal right]]]
    (let [pat (like->pattern right)
          left-f (eval-fn left)]
      #(when-some [left (left-f %)]
         (some? (re-matches pat (str left)))))

    [["if" pred-expr true-expr false-expr]]
    (let [pred-f (eval-fn pred-expr)
          true-f (eval-fn true-expr)
          false-f (eval-fn false-expr)]
      #(if (pred-f %)
         (true-f %)
         (false-f %)))

    [["coalesce" & exprs]]
    (let [fs (mapv eval-fn exprs)]
      #(loop [[f & fs] fs]
         (when f
           (if-some [res (f %)]
             res
             (recur fs)))))

    ["null"]
    (constantly nil)

    [[nested]]
    (eval-fn nested)))

(defn eval [out-path expr]
  (let [f (eval-fn expr)]
    (map (fn [r] (assoc|dissoc-in r out-path (f r))))))

(defn- transaction-merge [& records]
  (let [raw (when-some [records (seq (keep #(get % "_raw") records))]
              (str/join "\n" records))
        merged (apply merge records)]
    (assoc|dissoc merged "_raw" raw)))

(defn- within-maxspan? [^Duration maxspan start current]
  (or (nil? maxspan)
      (let [start (get "_time" start)
            current (get "_time" current)]
        (if (and start current)
          (neg? (.compareTo (Duration/between start current) maxspan))
          (throw (ex-info "transaction: maxspan specified but _time missing!"
                          {:type :runtime
                           :command :transaction}))))))

(defn transaction [path {:keys [startswith endswith maxevents maxspan keeporphans]}]
  (let [startswith (and startswith (search-predicate [:search-expression startswith]))
        endswith (and endswith (search-predicate [:search-expression endswith]))
        maxevents (when (and maxevents (pos? maxevents)) maxevents)]
    (fn [rf]
      (let [all-open (java.util.HashMap.)
            handle-orphan (if keeporphans
                            (fn [result input]
                              (rf result (assoc input "_txn_orphan" 1)))
                            (fn [result _input]
                              result))]
        (fn
          ([] (rf))
          ([result]
           (if endswith
               ;; we can't close any more
             (rf result)
               ;; everything gets closed
             (let [result ;; TODO: correct order
                   (unreduced (reduce rf result (map (partial apply transaction-merge) (vals all-open))))]
               (rf result))))
          ([result input]
           (if-some [k (get-in input path)]
               ;; FIXME str is too generic? how should we match these
             (let [k (str k)]
               (if-some [open (.get all-open k)]
                 (if (and (within-maxspan? maxspan (first open) input)
                          (or (nil? maxevents) (< (count open) maxevents)))
                   (if (and endswith (endswith input))
                     (do (.remove all-open k)
                         (rf result (apply transaction-merge (conj open input))))
                     (do (.put all-open k (conj open input))
                         result))
                   (let [result (if (nil? endswith)
                                  (rf result (apply transaction-merge open))
                                    ;; this might mess up the order, should we fix that?
                                  (unreduced (reduce handle-orphan result open)))]
                     (if (or (nil? startswith) (startswith input))
                       (.put all-open k [input])
                       (do
                         (.remove all-open k)
                         (handle-orphan result input)))
                     result))
                 (if (or (nil? startswith) (startswith input))
                   (do (.put all-open k [input])
                       result)
                   (handle-orphan result input))))
             (handle-orphan result input))))))))

(defn- hash-subquery [path input subquery]
  (->> input
       (sequence subquery)
       (reduce (fn [acc v]
                 (if-some [k (get-in v path)]
                   (update acc k (fnil conj []) v)
                   acc))
               {})))

(defn join [path subquery-input subquery {:keys [type overwrite max]}]
  (let [sq (future (hash-subquery path subquery-input subquery))
        outer? (#{"outer" "left"} type)
        ;; default 1, 0 means no limit
        max (if (= 0 max)
              nil
              (or max 1))]
    (fn [rf]
      (fn
        ([] (rf))
        ([result] (rf result))
        ([result input]
         (if-some [k (get-in input path)]
           (if-some [rs (get @sq k)]
             (let [rs (if max (take max rs) rs)]
               (if overwrite
                 (rf result (apply merge input rs))
                 (rf result (apply merge (conj rs input)))))
             (if outer?
               (rf result input)
               result))
           result))))))

(defn rex
  ([pattern]
   (rex pattern "_raw"))
  ([pattern path]
   ;; https://stackoverflow.com/questions/15588903
   (let [groups (mapv second (re-seq #"\(\?<([a-zA-Z][a-zA-Z0-9]*)>" pattern))
         pattern (re-pattern pattern)]
     (keep
      (fn [r]
        (when-some [value (get-in r path)]
          (let [matcher (re-matcher pattern value)]
            (when (re-find matcher)
              (update r :fields into (map (fn [^String g] [g (.group matcher g)]) groups))))))))))

(defn fillnull [paths default]
  (let [paths (mapv second paths)]
    (map #(reduce (fn [acc p] (if (nil? (get-in % p))
                                (assoc-in acc p default)
                                acc))
                  % paths))))

(defn dedup [n paths]
  (let [paths (mapv second paths)]
    (fn [rf]
      (let [seen-counts (java.util.HashMap.)]
        (fn
          ([] (rf))
          ([result] (rf result))
          ([result input]
           (let [k (mapv #(get-in input %) paths)
                 cnt (.getOrDefault seen-counts k 0)]
             (if (= cnt n)
               result
               (do (.put seen-counts k (inc cnt))
                   (rf result input))))))))))

(defn fields-with [paths]
  (let [paths (mapv second paths)]
    (map (fn [item]
           (reduce (fn [acc path]
                     (if-some [v (get-in item path)]
                       (assoc-in acc path v)
                       acc)) {} paths)))))

(defn fields-without [paths]
  (let [paths (mapv second paths)]
    (map (fn [item]
           (reduce (fn [acc path] (dissoc-in acc path)) item paths)))))

(def internal-fields #{["_raw"]})         ; can't be removed by `fields`

(defn compile-commands
  "Takes a sequence of commands (as parsed by splunge.query) and input for any
  subqueries and returns a transducer that can be applied to that same input. If
  no subqueries are present, no reference to the input is kept."
  [commands subquery-input]
  (letfn [(compile-command [command]
            (match [command]
              [[:command "search" & args]]
              (search args)

              [[:command "stats" [:stats-fn-list & stats-fns] [:field-list & paths]]]
              (stats stats-fns paths)

              [[:command "stats" [:stats-fn-list & stats-fns]]]
              (stats stats-fns [])

              [[:command "rex" [:rex-pattern pat]]]
              (rex pat)

              [[:command "rex" [:path path] [:rex-pattern pat]]]
              (rex pat path)

              [[:command "eval" [:path out-path] & expr]]
              (eval out-path expr)

              [[:command "join" [:path on-path] [:subquery & sq] & opts]]
              (join on-path subquery-input (compile-commands sq subquery-input)
                    (into {} (map (fn [[_ k v]] [(keyword k) v]) opts)))

              [[:command "transaction" [:path on-path] & opts]]
              (transaction on-path
                           (into {} (map (fn [[_ k v]] [(keyword k) v]) opts)))

              [[:command "fields" "-" & paths]]
              (fields-without (remove internal-fields paths))

              [(:or [:command "fields" "+" [:field-list & paths]]
                    [:command "fields" [:field-list & paths]])]
              (fields-with (concat paths internal-fields))

              [[:command "table" [:field-list & paths]]]
              (fields-with paths)

              [[:command "sort" & sort-bys]]
              (_sort sort-bys)

              [[:command "fillnull" "value" [:literal v] [:field-list & paths]]]
              (fillnull paths v)

              [[:command "fillnull" [:field-list & paths]]]
              (fillnull paths 0)

              [[:command "dedup" (n :guard number?) [:field-list & paths]]]
              (dedup n paths)

              [[:command "dedup" [:field-list & paths]]]
              (dedup 1 paths)))]

    (apply comp (map compile-command commands))))
