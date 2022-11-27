(ns caesarhu.project-euler.euler-010
  (:require [caesarhu.math.primes :refer [primes]]))

(defn euler-010
  [n]
  (apply + (primes n)))

(comment
  (time (euler-010 2000000))
  )