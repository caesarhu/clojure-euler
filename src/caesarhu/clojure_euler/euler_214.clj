(ns caesarhu.clojure-euler.euler-214
  (:require [caesarhu.math.primes :as p]))

(comment
  (time (->> (p/primes 40000000)
             count))
  )
