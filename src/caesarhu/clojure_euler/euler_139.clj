(ns caesarhu.clojure-euler.euler-139
  (:require [caesarhu.math.math-tools :refer [pythagorean-triplet]]
            [caesarhu.math.pell-equation :refer [pell-solutions]]
            [clojure.math.numeric-tower :refer [expt]]))

(defn brute-force
  [limit]
  (->> (pythagorean-triplet limit)
       (filter (fn [[a b c]]
                 (zero? (mod c (abs (- a b))))))
       (map #(apply + %))
       (map #(quot limit %))
       (apply +)))

(defn euler-139
  [limit]
  (->> (pell-solutions 2 -1)
       rest
       (map #(apply + %))
       (take-while #(<= % limit))
       (map #(quot limit %))
       (apply +)))

(comment
  (time (brute-force (expt 10 8)))
  (time (euler-139 (expt 10 8)))
  )