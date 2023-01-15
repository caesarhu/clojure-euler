(ns caesarhu.clojure-euler.euler-129
  (:require [clojure.math.numeric-tower :refer [gcd]]
            [caesarhu.math.primes :as p]))

(defn repunit
  [n]
  (if (= 1 (gcd 10 n))
    (loop [x 1, k 1]
      (if (zero? (mod x n))
        k
        (recur (mod (inc (* x 10)) n) (inc k))))
    0))

(def repunit-seq
  (->> (p/primes)
       (map #(vector % (repunit %)))))

(defn euler-129
  [limit]
  (->> (iterate inc (inc limit))
       (some #(and (> (repunit %) limit) %))))

(comment
  (time (euler-129 1000000))
  )