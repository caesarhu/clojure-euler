(ns caesarhu.project-euler.euler-053
  (:require [caesarhu.math.math-tools :refer [binomial]]))

(defn euler-053
  []
  (->> (for [i (range 1 101)
             j (range 1 i)
             :when (> (binomial i j) 1000000)]
         1)
       (apply +)))

(comment
  (time (euler-053))
  )