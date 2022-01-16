(ns caesarhu.project-euler.euler-007
  (:require [caesarhu.math.primes :refer [primes]]))

(defn euler-007
  [n]
  (nth primes (dec n)))

(comment
  (euler-007 10001)
  )