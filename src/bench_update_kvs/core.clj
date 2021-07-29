(ns bench-update-kvs.core)

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
                  (fn [afn]
                    (average-time-ms 8 amt-per-iter afn))
                  afn-map)
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

  (let [data {0 1 2 3 4 5 6 7}
        size 1000]
      (run-benchmark "transform keys/vals of a small map" iterations
                     (update-keys0  inc data)
                     (update-vals0  inc data)
                     (update-keys-rkv inc data)
                     (update-vals-rkv inc data)
                     (update-keys-itr inc data)
                     (update-vals-itr inc data)
                     (update-keys-trns inc data)
                     (update-vals-trns inc data))

      (let [data (->> (for [i (range size)] [i i]) (into {}))]
        (run-benchmark "transform values of large map" (/ iterations size)
                       (update-keys0  inc data)
                       (update-vals0  inc data)
                       (update-keys-rkv inc data)
                       (update-vals-rkv inc data)
                       (update-keys-itr inc data)
                       (update-vals-itr inc data)
                       (update-keys-trns inc data)
                       (update-vals-trns inc data))))
  (doseq [fun '[update-keys0 update-vals0 update-keys-rkv update-vals-rkv update-keys-itr update-vals-itr update-keys-trns update-vals-trns]
          m    [(hash-map 0 1 2 3) (array-map 0 1 2 3) (sorted-map 2 3 0 1)]]
    (let [f (resolve fun)
          r (f inc m)]
      (if (= (type m) (type r))
        (println fun " preserves type " (type r))
        (println fun " CHANGES type " (type m) " TO " (type r)))))
)

(comment
  (type (first (sorted-map 2 3 0 1)))
  
  (update-keys0 name {:a 1 :b 2})
  (update-vals0 inc {:a 1 :b 2})
  (update-keys1 name {:a 1 :b 2})
  (update-vals1 inc {:a 1 :b 2})
  (update-keys2 name {:a 1 :b 2})
  (update-vals2 inc {:a 1 :b 2})
  (update-keys3 name {:a 1 :b 2})
  (update-vals3 inc {:a 1 :b 2})
)
