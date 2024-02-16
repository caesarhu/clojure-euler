(ns caesarhu.clojure-euler.euler-187
  (:require [caesarhu.math.primes :as p]
            [clojure.math.numeric-tower :refer [expt]]))

(defn euler-187
  [limit]
  (loop [primes (vec (p/primes (/ limit 2)))
         sum 0]
    (let [counter (count primes)]
      (cond
        (zero? counter) sum
        (= counter 1) (inc sum)
        :else (let [upper (/ limit (second primes))
                    i (if-let [x (some #(and (< (primes %) upper) %) (iterate dec (dec counter)))] x 0)
                    new-primes (subvec primes 1 (inc i))]
                (recur new-primes (+ sum counter)))))))

(comment
  (time (euler-187 (expt 10 8)))
  )
