(ns caesarhu.clojure-euler.euler-077
  (:require [caesarhu.math.primes :as p]
            [clojure.core.memoize :as m]))

(defn count-prime-sums
  [n prime-seq]
  (cond
    (or (neg? n) (empty? prime-seq)) 0
    (zero? n) 1
    :else (+ (count-prime-sums n (rest prime-seq))
             (count-prime-sums (- n (first prime-seq)) prime-seq))))

(defn prime-sums
  [n]
  (count-prime-sums n (p/primes n)))

(defn euler-077
  [limit]
  (->> (iterate inc 1)
       (some #(and (> (prime-sums %) limit) %))))

(comment
  (time (euler-077 5000))
  )