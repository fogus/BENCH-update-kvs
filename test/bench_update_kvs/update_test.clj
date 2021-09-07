(ns bench-update-kvs.update-test
  (:require clojure.data.priority-map
            clojure.data.int-map
            clojure.data.avl
            [bench-update-kvs.disp :as dispatch])
  (:use clojure.test
        bench-update-kvs.core))

(deftest test-update-kvs
  (let [inm  (with-meta {:a 1 :b 2} {:has :meta})]
    (are [result expr] (= result expr)
      {"a" 1 "b" 2} (dispatch/update-keys inm name)
      {:has :meta}  (meta (dispatch/update-keys inm name))
      {:a 2 :b 3}   (dispatch/update-vals inm inc)
      {:has :meta}  (meta (dispatch/update-vals inm inc)))))

(deftest test-structs
  (doseq [fun [dispatch/update-keys dispatch/update-vals]
          m    [(hash-map 0 1 2 3) (array-map 0 1 2 3)
                (sorted-map 2 3 0 1)
                (clojure.data.priority-map/priority-map 0 1 2 3)
                (clojure.data.int-map/int-map (int 0) (int 1) (int 2) (int 3))
                (clojure.data.avl/sorted-map 0 1 2 3)]]
    (println "Checking type " (type m) " against " fun)
    (fun m inc)))
