(ns bench-update-kvs.disp
  (:require [bench-update-kvs.core :as b]))

(set! *warn-on-reflection* true)

(defn reduce-kv2
  "Reduces an associative collection. f should be a function of 3
  arguments. Returns the result of applying f to init, the first key
  and the first value in coll, then applying f to that result and the
  2nd key and value, etc. If coll contains no entries, returns init
  and f is not called. Note that reduce-kv is supported on vectors,
  where the keys will be the ordinals."  
  {:added "1.4"}
  ([f init coll]
   (if (instance? clojure.lang.IKVReduce coll)
     (.kvreduce ^clojure.lang.IKVReduce coll f init)
     (clojure.core.protocols/kv-reduce coll f init))))

(extend-protocol clojure.core.protocols/IKVReduce
  ;;slow path default
  clojure.lang.IPersistentMap
  (kv-reduce
    [amap f init]
    (swap! b/slowpaths inc)
    (reduce (fn [ret [k v]] (f ret k v)) init amap)))

(defn update-keys
  "m f => {(f k) v ...}

  Given a map m and a function f of 1-argument, returns a new map whose
  keys are the result of applying f to the keys of m, mapped to the
  corresponding values of m.
  f must return a unique key for each key of m."
  {:added "1.11"}
  [m f]
  (let [ret (persistent!
             (reduce-kv (fn [acc k v] (assoc! acc (f k) v))
                         (transient {})
                         m))]
    (if (= (count m) (count ret))
      (with-meta ret (meta m))
      (throw (RuntimeException. "Key transform function did not return unique values.")))))

(defn update-vals
  "m f => {k (f v) ...}

  Given a map m and a function f of 1-argument, returns a new map where the keys of m
  are mapped to result of applying f to the corresponding values of m."
  {:added "1.11"}
  [m f]
  (with-meta
    (persistent!
     (reduce-kv (fn [acc k v] (assoc! acc k (f v)))
                 (transient m)
                 m))
    (meta m)))

(defn data [sz]
  (->> (for [i (range sz)] [i i]) (into {})))

(defn bench
  [{:keys [iterations] :as opts :or {iterations 1000000}}]
  (println "  Clojure version " *clojure-version*)
  (println "  Java version " (System/getProperty "java.version"))

  (let [size-sm  10
        size-md  100
        size-lg  1000
        size-xl  10000
        size-xxl 100000]
    (let []
      (b/run-benchmark "run update-keys" iterations
                       (update-keys (data size-sm) inc)
                       (update-keys (data size-md) inc))

      (b/run-benchmark "run update-vals" iterations
                       (update-vals (data size-sm) inc)
                       (update-vals (data size-md) inc)))))
