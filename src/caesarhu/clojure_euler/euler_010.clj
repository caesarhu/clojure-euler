(ns caesarhu.clojure-euler.euler-010
  (:require [caesarhu.math.primes :refer [primes]]))

(defn euler-010
  [n]
  (apply + (primes n)))

(comment
  (time (count (primes 2000000)))
  )
