(ns bench-update-kvs.disp
  (:require [bench-update-kvs.core :as b]))

(set! *warn-on-reflection* true)

(extend-protocol clojure.core.protocols/IKVReduce
  nil
  (kv-reduce
    [_ f init]
    init)

  ;;slow path default
  java.lang.Object
  (kv-reduce
    [amap f init]
    (reduce (fn [ret ^java.util.Map$Entry me]
              (f ret
                 (.getKey me)
                 (.getValue me)))
            init
            amap))
  
  clojure.lang.IKVReduce
  (kv-reduce
    [amap f init]
    (.kvreduce amap f init)))

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
    (let [data (->> (for [i (range size-sm)] [i i]) (into {}))]
      (b/run-benchmark "run update-keys" iterations
                       (update-keys data inc)))

    (let [data (->> (for [i (range size-md)] [i i]) (into {}))]
      (b/run-benchmark "run update-keys" (/ iterations size-sm)
                       (update-keys data inc)))

    (let [data (->> (for [i (range size-lg)] [i i]) (into {}))]
      (b/run-benchmark (str "transform keys of a map (" (count data) " keys)") (/ iterations size-md)
                       (update-keys data inc)))
    
    (let [data (->> (for [i (range size-xl)] [i i]) (into {}))]
      (b/run-benchmark (str "transform keys of a map (" (count data) " keys)") (/ iterations size-md)
                       (update-keys data inc)))

    (let [data (->> (for [i (range size-xxl)] [i i]) (into {}))]
      (b/run-benchmark (str "transform keys of a map (" (count data) " keys)") (/ iterations size-md)
                     (update-keys data inc)))

    ;; vals
    (let [data (->> (for [i (range size-sm)] [i i]) (into {}))]
      (b/run-benchmark "run update-vals" iterations
                       (update-vals data inc)))

    (let [data (->> (for [i (range size-md)] [i i]) (into {}))]
      (b/run-benchmark "run update-vals" (/ iterations size-sm)
                       (update-vals data inc)))

    (let [data (->> (for [i (range size-lg)] [i i]) (into {}))]
      (b/run-benchmark (str "transform vals of a map (" (count data) " keys)") (/ iterations size-md)
                       (update-vals data inc)))
    
    (let [data (->> (for [i (range size-xl)] [i i]) (into {}))]
      (b/run-benchmark (str "transform vals of a map (" (count data) " keys)") (/ iterations size-md)
                       (update-vals data inc)))

    (let [data (->> (for [i (range size-xxl)] [i i]) (into {}))]
      (b/run-benchmark (str "transform vals of a map (" (count data) " keys)") (/ iterations size-md)
                     (update-vals data inc)))

    ))
