(ns caesarhu.project-euler.euler-047
  (:require [caesarhu.math.primes :as p]))

(defn prime-factors
  [n]
  (-> (p/factors n) distinct count))

(defn euler-047
  [n]
  (let [target (repeat n n)
        count-seq (->> (iterate inc 2)
                       (map prime-factors)
                       (partition n 1))]
    (->> (map vector count-seq (iterate inc 2))
         (filter #(= target (first %)))
         first
         last)))

(comment
  (time (euler-047 4))
  )