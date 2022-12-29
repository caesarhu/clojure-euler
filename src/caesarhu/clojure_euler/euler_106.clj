(ns caesarhu.clojure-euler.euler-106
  (:require [clojure.math.combinatorics :as combo]))

(defn binomial
  [n k]
  (let [fact #(apply *' (range 1 (inc %)))]
    (/ (fact n)
       (*' (fact k) (fact (- n k))))))

(defn euler-106
  [n]
  (->> (for [i (range 2 (inc (quot n 2)))]
         (/ (* (binomial n (* 2 i))
               (binomial (* 2 i) i)
               (dec i))
            (+ 2 (* 2 i))))
       (apply +)))

(comment
  (euler-106 12)
  )