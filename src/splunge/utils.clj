(ns splunge.utils
  (:require [clojure.core.incubator :refer [dissoc-in]]))

(defn assoc|dissoc [m k v]
  (if (some? v)
    (assoc m k v)
    (dissoc m k)))

(defn assoc|dissoc-in [m ks v]
  (if (some? v)
    (assoc-in m ks v)
    (dissoc-in m ks)))
