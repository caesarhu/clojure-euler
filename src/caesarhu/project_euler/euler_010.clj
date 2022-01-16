(ns caesarhu.project-euler.euler-010
  (:require [caesarhu.math.primes :refer [primes]]))

(defn euler-010
  [n]
  (->> (take-while #(< % n) primes)
       (apply +)))

(comment
  (time (euler-010 2000000))
  )