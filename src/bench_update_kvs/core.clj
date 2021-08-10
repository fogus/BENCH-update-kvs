(ns bench-update-kvs.core
  (:require clojure.data.priority-map
            clojure.data.int-map
            clojure.data.avl))

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
     (.kvreduce coll f init)
     (clojure.core.protocols/kv-reduce coll f init))))

(defn update-keys-naive
  "m f => {(f k) v ...}

  Given a map m and a function f of 1-argument, returns a new map whose
  keys are the result of applying f to the keys of m, mapped to the
  corresponding values of m.
  f must return a unique key for each key of m."
  {:added "1.11"}
  [m f]
  (let [ret (with-meta
              (zipmap (map f (keys m)) (vals m))
              (meta m))]
    (if (= (count m) (count ret))
      ret
      (throw (RuntimeException. "Key transform function did not return unique values.")))))

(defn update-vals-naive
  "m f => {k (f v) ...}

  Given a map m and a function f of 1-argument, returns a new map where the keys of m
  are mapped to result of applying f to the corresponding values of m."
  {:added "1.11"}
  [m f]
  (with-meta
    (zipmap (keys m) (map f (vals m)))
    (meta m)))

(defn update-keys-red
  "reduce, assumes seq of Map.Entry, builds [[k v]...] as input to into"
  {:added "1.11"}
  [m f]
  (let [ret (into (with-meta {} (meta m))
                  (reduce
                   (fn [acc ^java.util.Map$Entry me] (conj acc [(f (.getKey me)) (.getValue me)]))
                   []
                   m))]
    (if (= (count m) (count ret))
      ret
      (throw (RuntimeException. "Key transform function did not return unique values.")))))

(defn update-vals-red
  "reduce, assumes seq of Map.Entry, calls out to update"
  {:added "1.11"}
  [m f]
  (with-meta 
   (reduce
    (fn [acc ^java.util.Map$Entry me] (update acc (.getKey me) f))
    m
    m)
    (meta m)))

(defn update-keys-rkv
  "reduce-kv, builds [[k v]...] as input to into"
  {:added "1.11"}
  [m f]
  (into (with-meta {} (meta m))
        (reduce-kv (fn [acc k v] (conj acc [(f k) v]))
                   []
                   m)))

(defn update-vals-rkv
  "reduce-kv calls out to update"
  {:added "1.11"}
  [m f]
  (reduce-kv (fn [acc k _] (update acc k f))
             m
             m))

(defn update-keys-rkv2
  "reduce-kv, builds [[k v]...] as input to into"
  {:added "1.11"}
  [m f]
  (into (with-meta {} (meta m))
        (reduce-kv2 (fn [acc k v] (conj acc [(f k) v]))
                    []
                    m)))

(defn update-vals-rkv2
  "reduce-kv calls out to update"
  {:added "1.11"}
  [m f]
  (reduce-kv2 (fn [acc k _] (update acc k f))
              m
              m))

(defn update-keys-trns
  "Transducer version, builds [[k v]...] as input to into"
  {:added "1.11"}
  [m f]
    (let [ret (into (with-meta {} (meta m))
                    (map (fn [[k v]] [(f k) v]))
                    m)]
    (if (= (count m) (count ret))
      ret
      (throw (RuntimeException. "Key transform function did not return unique values.")))))

(defn update-vals-trns
  "Transducer version, builds [[k v]...] as input to into"
  {:added "1.11"}
  [m f]
  (into (with-meta {} (meta m))
        (map (fn [[k v]] [k (f v)]))
        m))


;;; BENCH STUFF

(defn pretty-float5 [anum]
  (format "%.5g" anum))

(defn pretty-float3 [anum]
  (format "%.3g" anum))

(defn time-ms [amt afn]
  (let [start (System/nanoTime)
        _ (dotimes [_ amt] (afn))
        end (System/nanoTime)]
  (/ (- end start) 1000000.0)
  ))

(defn avg [numbers]
  (/ (reduce + numbers)
     (count numbers)
     1.0))

(defn average-time-ms [iters amt-per-iter afn]
  (avg
    ;; treat 1st run as warmup 
    (next
      (for [i (range (inc iters))]
        (time-ms amt-per-iter afn)))))

(def ^:dynamic *TIMES* 8)

(defn compare-benchmark [amt-per-iter afn-map]
  (let [results (update-vals-naive
                 afn-map
                 (fn [afn]
                   (average-time-ms *TIMES* amt-per-iter afn)))
        [[_ best-time] & _ :as sorted] (sort-by last results)
        ]
    (println "\nAvg(ms)\t\tvs best\t\tCode")
    (doseq [[k t] sorted]
      (println (pretty-float5 t) "\t\t" (pretty-float3 (/ t best-time 1.0)) "\t\t" k)
      )))

(defmacro run-benchmark [name amt-per-iter & exprs]
  (let [afn-map (->> exprs shuffle (map (fn [e] [`(quote ~e) `(fn [] ~e)])) (into {}))]
    `(do
       (println "Benchmark:" ~name (str "(" (* *TIMES* ~amt-per-iter) " iterations with " ~amt-per-iter " warmup)"))
       (compare-benchmark ~amt-per-iter ~afn-map)
       (println "\n********************************\n")
       )))

(defn bench
  [{:keys [iterations] :as opts :or {iterations 1000000}}]
  (println "  Clojure version " *clojure-version*)

  (let [size-sm 10
        size-md 100
        size-lg 1000]
    (let [data (->> (for [i (range size-sm)] [i i]) (into {}))]
      (run-benchmark (str "transform keys of a map (" (count data) " keys)") iterations
                     (update-keys-naive  data inc)
                     (update-keys-red data inc)
                     (update-keys-rkv data inc)
                     (update-keys-rkv2 data inc)
                     (update-keys-trns data inc)))
    
    (let [data (->> (for [i (range size-sm)] [i i]) (into {}))]
      (run-benchmark (str "transform vals of a map (" (count data) " keys)") iterations
                     (update-vals-naive  data inc)
                     (update-vals-rkv data inc)
                     (update-vals-rkv2 data inc)
                     (update-vals-red data inc)
                     (update-vals-trns data inc)))

    (let [data (->> (for [i (range size-md)] [i i]) (into {}))]
      (run-benchmark (str "transform keys of a map (" (count data) " keys)") (/ iterations size-sm)
                     (update-keys-naive  data inc)
                     (update-keys-red data inc)
                     (update-keys-rkv data inc)
                     (update-keys-rkv2 data inc)
                     (update-keys-trns data inc)))

    (let [data (->> (for [i (range size-md)] [i i]) (into {}))]
      (run-benchmark (str "transform vals of a map (" (count data) " keys)") (/ iterations size-sm)
                     (update-vals-naive  data inc)
                     (update-vals-red data inc)
                     (update-vals-rkv data inc)
                     (update-vals-rkv2 data inc)
                     (update-vals-trns data inc)))

    (let [data (->> (for [i (range size-lg)] [i i]) (into {}))]
      (run-benchmark (str "transform keys of a map (" (count data) " keys)") (/ iterations size-md)
                     (update-keys-naive  data inc)
                     (update-keys-red data inc)
                     (update-keys-rkv data inc)
                     (update-keys-rkv2 data inc)
                     (update-keys-trns data inc)))

    (let [data (->> (for [i (range size-lg)] [i i]) (into {}))]
      (run-benchmark (str "transform vals of a map (" (count data) " keys)") (/ iterations size-md)
                     (update-vals-naive  data inc)
                     (update-vals-red data inc)
                     (update-vals-rkv data inc)
                     (update-vals-rkv2 data inc)
                     (update-vals-trns data inc))))
  
  (doseq [fun [update-keys-naive update-vals-naive update-keys-red update-vals-red update-keys-rkv update-vals-rkv update-keys-rkv2 update-vals-rkv2 update-keys-trns update-vals-trns]
          m    [(hash-map 0 1 2 3) (array-map 0 1 2 3) (sorted-map 2 3 0 1)
                (clojure.data.priority-map/priority-map 0 1 2 3)
                (clojure.data.int-map/int-map (int 0) (int 1) (int 2) (int 3))
                (clojure.data.avl/sorted-map 0 1 2 3)]]
    (println "Checking type " (type m) " against " fun)
    (fun m inc)))

