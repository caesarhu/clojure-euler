(ns caesarhu.project-euler.euler-012
  (:require [caesarhu.math.primes :as p]))

(defn triangle-number
  [n]
  (quot (* n (inc n)) 2))

(defn euler-012
  [limit]
  (->> (map triangle-number (iterate inc 1))
       (filter #(> (p/count-divisors %) limit))
       first))

(comment
  (time (euler-012 500))
  )