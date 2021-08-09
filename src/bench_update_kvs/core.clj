(ns bench-update-kvs.core
  (:require clojure.data.priority-map
            clojure.data.int-map
            clojure.data.avl))

(defn update-keys0
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

(defn update-vals0
  "m f => {k (f v) ...}

  Given a map m and a function f of 1-argument, returns a new map where the keys of m
  are mapped to result of applying f to the corresponding values of m."
  {:added "1.11"}
  [m f]
  (with-meta
    (zipmap (keys m) (map f (vals m)))
    (meta m)))

(defn update-keys-red
  "reduce Map.Entry version"
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
  "reduce Map.Entry version"
  {:added "1.11"}
  [m f]
  (into (with-meta {} (meta m))
   (reduce
    (fn [acc ^java.util.Map$Entry me] (update acc (.getKey me) f))
    m
    m)))

(defn update-vals-rkv
  "reduce-kv"
  {:added "1.11"}
  [m f]
  (reduce-kv (fn [acc k _] (update acc k f))
             m
             m))

(defn update-keys-trns
  "Transducer version"
  {:added "1.11"}
  [m f]
    (let [ret (into (with-meta {} (meta m))
                    (map (fn [[k v]] [(f k) v]))
                    m)]
    (if (= (count m) (count ret))
      ret
      (throw (RuntimeException. "Key transform function did not return unique values.")))))

(defn update-vals-trns
  "Transducer version"
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
  (let [_ (dotimes [_ (/ amt 2)] (afn))
        start (System/nanoTime)
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

(defn compare-benchmark [amt-per-iter afn-map]
  (let [results (update-vals0
                 afn-map
                 (fn [afn]
                   (average-time-ms 8 amt-per-iter afn)))
        [[_ best-time] & _ :as sorted] (sort-by last results)
        ]
    (println "\nAvg(ms)\t\tvs best\t\tCode")
    (doseq [[k t] sorted]
      (println (pretty-float5 t) "\t\t" (pretty-float3 (/ t best-time 1.0)) "\t\t" k)
      )))

(defmacro run-benchmark [name amt-per-iter & exprs]
  (let [afn-map (->> exprs shuffle (map (fn [e] [`(quote ~e) `(fn [] ~e)])) (into {}))]
    `(do
       (println "Benchmark:" ~name (str "(" ~amt-per-iter " iterations, with half that amount warmup)"))
       (compare-benchmark ~amt-per-iter ~afn-map)
       (println "\n********************************\n")
       )))

(defn bench
  [{:keys [iterations] :as opts :or {iterations 1000000}}]
  (println "Benchmarking with " iterations "iterations.")
  (println "  Clojure version " *clojure-version*)

  (let [size-sm 10
        size-md 100
        size-lg 1000]
    (let [data (->> (for [i (range size-sm)] [i i]) (into {}))]
      (run-benchmark (str "transform keys of a map (" (count data) " keys)") iterations
                     (update-keys0  data inc)
                     (update-keys-red data inc)
                     (update-keys-trns data inc)))
    
    (let [data (->> (for [i (range size-sm)] [i i]) (into {}))]
      (run-benchmark (str "transform vals of a map (" (count data) " keys)") iterations
                     (update-vals0  data inc)
                     (update-vals-rkv data inc)
                     (update-vals-red data inc)
                     (update-vals-trns data inc)))

    (let [data (->> (for [i (range size-md)] [i i]) (into {}))]
      (run-benchmark (str "transform keys of a map (" (count data) " keys)") (/ iterations size-md)
                     (update-keys0  data inc)
                     (update-keys-red data inc)
                     (update-keys-trns data inc)))

    (let [data (->> (for [i (range size-md)] [i i]) (into {}))]
      (run-benchmark (str "transform vals of a map (" (count data) " keys)") (/ iterations size-md)
                     (update-vals0  data inc)
                     (update-vals-red data inc)
                     (update-vals-rkv data inc)
                     (update-vals-trns data inc)))

    (let [data (->> (for [i (range size-lg)] [i i]) (into {}))]
      (run-benchmark (str "transform keys of a map (" (count data) " keys)") (/ iterations size-lg)
                     (update-keys0  data inc)
                     (update-keys-red data inc)
                     (update-keys-trns data inc)))

    (let [data (->> (for [i (range size-lg)] [i i]) (into {}))]
      (run-benchmark (str "transform vals of a map (" (count data) " keys)") (/ iterations size-lg)
                     (update-vals0  data inc)
                     (update-vals-red data inc)
                     (update-vals-rkv data inc)
                     (update-vals-trns data inc))))
  
  (doseq [fun [update-keys0 update-vals0 update-keys-red update-vals-red update-vals-rkv update-keys-trns update-vals-trns]
          m    [(hash-map 0 1 2 3) (array-map 0 1 2 3) (sorted-map 2 3 0 1)
                (clojure.data.priority-map/priority-map 0 1 2 3)
                (clojure.data.int-map/int-map (int 0) (int 1) (int 2) (int 3))
                (clojure.data.avl/sorted-map 0 1 2 3)]]
    (println "Checking type " (type m) " against " fun)
    (fun m inc)))

