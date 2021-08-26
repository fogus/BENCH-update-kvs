(ns bench-update-kvs.context-free
  (:require [bench-update-kvs.core :as b]
            clojure.data.priority-map
            clojure.data.int-map
            clojure.data.avl))

(set! *warn-on-reflection* true)

(defn slow-branch [amap f init]
  (reduce (fn [ret [k v]] (f ret k v)) init amap))

(defn fast-branch [^clojure.lang.IKVReduce amap f init]
  (.kvreduce amap f init))

(defn me-branch [amap f init]
  (reduce (fn [ret ^java.util.Map$Entry me]
            (f ret
               (.getKey me)
               (.getValue me)))
          init
          amap))

(defn bench
  [{:keys [iterations] :as opts :or {iterations 1000000}}]
  (println "  Clojure version " *clojure-version*)

  (let [size-sm  10
        size-md  100
        size-lg  1000
        size-xl  10000
        size-xxl 100000]
    (let [data (->> (for [i (range size-lg)] [i i]) (into {}))
          fun  #(assoc %1 (inc %2) (inc %3))]
      (b/run-benchmark "run different branches of a kv-reduce proto aginst each other" iterations
                       (slow-branch data fun {})
                       (me-branch data fun {})
                       (fast-branch data fun {})))))
