Expansion of `(destructure-n pam 2 clojure.core.PersistentArrayMap/createAsIfByAssoc to-array)`

    (defn pam2 [iterations]
      (let [rhs (list :a0 0 :a1 1)]
        (loop [acc 0 n iterations]
          (if (pos? n)
            (let [{:keys [a0 a1]} (PersistentArrayMap/createAsIfByAssoc (to-array rhs))]
              (recur (+ acc (+ a0 a1))
                     (dec n)))
            acc))))
