(ns caesarhu.clojure-euler.euler-116
  (:require [caesarhu.math.math-tools :refer [binomial]]))

(defn count-color
  [n m]
  (->> (for [i (iterate inc 1)
             :while (<= (* i m) n)]
         (binomial (+ i (- n (* i m))) i))
       (apply +')))

(defn euler-116
  [n]
  (->> (map #(count-color n %) [2 3 4])
       (apply +')))

(comment
  (time (euler-116 50))
  )