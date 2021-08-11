(ns bench-update-kvs.update-test
  (:use clojure.test
        bench-update-kvs.core))

(deftest test-update-kvs
  (let [inm  (with-meta {:a 1 :b 2} {:has :meta})
        kfns '[update-keys-naive update-keys-red update-keys-rkv update-keys-rkv! update-keys-trns]
        vfns '[update-vals-naive update-vals-red update-vals-rkv update-vals-rkv! update-vals-trns]]
    (doseq [uk kfns
            uv vfns]
      (let [update-keys (ns-resolve (find-ns 'bench-update-kvs.core) uk)
            update-vals (ns-resolve (find-ns 'bench-update-kvs.core) uv)]
        (are [result expr] (= result expr)
          {"a" 1 "b" 2} (update-keys inm name)
          {:has :meta}  (meta (update-keys inm name))
          {:a 2 :b 3}   (update-vals inm inc)
          {:has :meta}  (meta (update-vals inm inc)))

        (println (str "Testing throws of " uk))
        (is (thrown? RuntimeException (update-keys inm (constantly :a))))))))

