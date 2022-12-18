(ns caesarhu.clojure-euler.euler-087
  (:require [caesarhu.math.primes :as p]))

(defn expt
  [n p]
  (apply * (repeat p n)))

(defn euler-087
  [limit]
  (let [power-primes (fn [p]
                       (take-while #(< % limit) (map #(expt % p) (p/primes))))
        p2 (power-primes 2)
        p3 (power-primes 3)]
    (->> (into #{}
               (comp
                (mapcat (fn [x] (map #(+ x %) p3)))
                (filter #(< % limit))
                (mapcat (fn [x] (map #(+ x %) p2)))
                (filter #(< % limit)))
               (power-primes 4))
         count)))

(comment
  (time (euler-087 50000000))
  )