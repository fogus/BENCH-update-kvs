(ns bench-update-kvs.disp)

(def C (atom 0))

;; reextend for instrumentation
(extend-protocol clojure.core.protocols/IKVReduce
  ;;slow path default
  clojure.lang.IPersistentMap
  (kv-reduce
    [amap f init]
    (swap! C inc)
    (reduce (fn [ret [k v]] (f ret k v)) init amap)))

(defn go [_]
  (reduce-kv (fn [_ _ _]) nil (array-map 1 2 3 4))
  (println "slowpaths" @C)
  (reduce-kv (fn [_ _ _]) nil (hash-map 1 2 3 4))
  (println "slowpaths" @C))
