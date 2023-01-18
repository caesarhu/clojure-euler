(ns caesarhu.clojure-euler.euler-136
  (:require [caesarhu.math.primes :as p]))

(defn euler-136
  [limit]
  (transduce (map (fn [p]
                    (cond-> 0
                      (< p (/ limit 4)) inc
                      (< p (/ limit 16)) inc
                      (= 3 (mod p 4)) inc)))
             + (p/primes limit)))

(comment
  (time (euler-136 50000000))
  )