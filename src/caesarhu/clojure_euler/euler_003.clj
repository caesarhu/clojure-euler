(ns caesarhu.clojure-euler.euler-003
  (:require [caesarhu.math.primes :refer [factors]]))

(defn euler-003
  [n]
  (apply max (factors n)))

(comment
  (time (euler-003 600851475143))
  )