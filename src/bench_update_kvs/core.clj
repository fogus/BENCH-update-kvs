(ns bench-update-kvs.core
  (:require clojure.data.priority-map
            clojure.data.int-map
            clojure.data.avl))

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
  
  (doseq [fun '[update-keys0 update-vals0 update-keys-red update-vals-red update-vals-rkv update-keys-trns update-vals-trns]
          m    [(hash-map 0 1 2 3) (array-map 0 1 2 3) (sorted-map 2 3 0 1)
                (clojure.data.priority-map/priority-map 0 1 2 3)
                (clojure.data.int-map/int-map (int 0) (int 1) (int 2) (int 3))
                (clojure.data.avl/sorted-map 0 1 2 3)]]
    (println "Checking type " (type m) " against " fun)
    (let [f (resolve fun)]
      (f m inc))))

