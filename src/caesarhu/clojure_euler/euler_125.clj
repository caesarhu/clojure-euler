(ns caesarhu.clojure-euler.euler-125
  (:require [caesarhu.math.math-tools :refer [palindrome?]]
            [clojure.math.numeric-tower :refer [sqrt]]))

(defn continued-square-sum
  [n]
  (/ (*' n (inc n) (inc (*' 2 n))) 6))

(defn euler-125
  [limit]
  (let [sqr (sqrt limit)
        square-sum-vec (vec (map continued-square-sum (range (inc sqr))))]
    (->> (for [i (range (- (count square-sum-vec) 2))]
           (for [j (range (+ i 2) (count square-sum-vec))
                 :let [diff (- (square-sum-vec j) (square-sum-vec i))]
                 :while (<= diff limit)
                 :when (palindrome? diff)]
             diff))
         flatten
         set
         (apply +))))

(comment
  (time (euler-125 100000000))
  )