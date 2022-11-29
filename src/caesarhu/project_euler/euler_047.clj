(ns caesarhu.project-euler.euler-047
  (:require [caesarhu.math.primes :as p]))

(def limit 200000)

(defn fill-factors
  [v]
  (let [limit (count @v)
        primes (p/primes limit)]
    (doseq [p primes
            i (rest (range))
            :let [p*i (* p i)]
            :while (< p*i limit)]
      (swap! v update p*i inc))))

(defn euler-047
  [n]
  (let [factor-vec (atom (vec (repeat limit 0)))
        _ (fill-factors factor-vec)]
    (loop [v @factor-vec
           i 0]
      (if (every? #(= n %) (take n v))
        i
        (recur (rest v) (inc i))))))

(comment
  (time (euler-047 4))
  )