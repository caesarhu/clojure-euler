(ns caesarhu.project-euler.euler-072
  (:require [caesarhu.math.primes :as p]
            [clojure.math.numeric-tower :as math]
            [caesarhu.math.math-tools :as tools]))

(defn brute-force
  [limit]
  (->> (map p/totient (range 2 (inc limit)))
       (apply +)))

;=================================================================

(defn totient-vector
  [limit]
  (let [limit+1 (inc limit)
        v (atom (vec (range limit+1)))]
    (doseq [i (range 2 limit+1)
            :when (= (@v i) i)]
      (doseq [j (take (quot limit i) (iterate #(+ % i) i))
              :let [vj (@v j)]]
        (swap! v assoc j (- vj (quot vj i)))))
    @v))

(defn totient-vector
  [limit]
  (loop [i 2
         result (vec (range limit+1))]
    (cond
      (> i limit) result
      (not= (result i) i) (recur (inc i) result)
      :else (recur (inc i) (loop [j i
                                  r result]
                             (if (> i limit)
                               r
                               (recur)))))))

(defn euler-072
  [limit]
  (apply + (drop 2 (totient-vector limit))))

(comment
  (totient-vector 10)
  (time (euler-072 1000000))
  (time (brute-force 1000000))
  )