(ns caesarhu.project-euler.euler-050
  (:require [caesarhu.math.primes :as p]))

(defn consecutive-primes
  [limit]
  (loop [counter (range)]
    (let [sum (apply + (take (first counter) (p/primes)))]
      (if (> sum limit)
        (take (dec (first counter)) (p/primes))
        (recur (rest counter))))))

(defn find-sum-prime
  [prime-seq]
  (let [sum-prime? (fn [coll] (p/is-prime? (apply + coll)))]
    (->> (mapcat #(partition % 1 prime-seq) (range (count prime-seq) 0 -1))
         (some #(and (sum-prime? %) %)))))

(defn euler-050
  [limit]
  (->> (consecutive-primes limit)
       (find-sum-prime)
       (#(hash-map :sum (apply + %) :values %))))

(comment
  (time (euler-050 1000000))
  )