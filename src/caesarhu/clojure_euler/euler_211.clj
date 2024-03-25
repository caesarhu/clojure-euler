(ns caesarhu.clojure-euler.euler-211
  (:require [caesarhu.math.primes :as p]
            [clojure.math.combinatorics :as c]
            [caesarhu.math.pell-equation :refer [pell-solutions]]
            [caesarhu.math.math-tools :refer [square? digits]]
            [clojure.math.numeric-tower :refer [exact-integer-sqrt expt]]))

(defn isqrt
  [^long n]
  (first (exact-integer-sqrt n)))

(defn square
  [^long n]
  (* n n))

(defn square-sum
  [^long limit]
  (let [v (atom [0 1])]
    (doseq [i (range 2 limit)]
      (swap! v assoc i (inc (square i))))
    (doseq [i (range 2 (isqrt limit))]
      (loop [m i
             j (* i m)]
        (when (< j limit)
          (if (= i m)
            (swap! v update j + (square i))
            (swap! v update j + (+ (square i) (square m))))
          (recur (inc m) (+ j i)))))
    @v))

(defn brute-force
  [^long limit]
  (let [sum-vec (square-sum limit)
        is-divisors-square? (fn [i]
                              (let [n (sum-vec i)]
                                (when (square? n)
                                  i)))
        result (keep is-divisors-square? (range 1 limit))]
    (apply + result)))

(comment
  (time (brute-force 64000000))
  )
