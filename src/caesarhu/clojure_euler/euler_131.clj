(ns caesarhu.clojure-euler.euler-131
  (:require [caesarhu.math.primes :as p]
            [clojure.math.numeric-tower :refer [expt]]))

(defn cube
  [n]
  (expt n 3))

(defn cube-primes
  [limit]
  (let [primes (set (p/primes limit))]
    (for [x (iterate inc 1)
          :let [y (inc x)
                p (- (cube y) (cube x))]
          :while (< p limit)
          :when (primes p)]
      p)))

(defn euler-131
  [limit]
  (count (cube-primes limit)))

(comment
  (time (euler-131 1000000))
  )