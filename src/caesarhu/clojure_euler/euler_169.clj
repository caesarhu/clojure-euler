(ns caesarhu.clojure-euler.euler-169
  (:require [caesarhu.math.math-tools :refer [digits]]
            [clojure.math.numeric-tower :refer [expt]]))

(defn euler-169
  [n]
  (loop [s (reverse (digits n 2))
         [p q] [0 1]]
    (if (not-empty s)
      (recur (rest s) (if (zero? (first s)) [(+ p q) q] [p (+ p q)]))
      q)))

(comment
  (time (euler-169 (expt 10 25)))
  )
