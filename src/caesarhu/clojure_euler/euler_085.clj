(ns caesarhu.clojure-euler.euler-085
  (:require [caesarhu.math.polynomial :refer [quadratic-root]]))

(defn triangular-number
  [n]
  (quot (* n (inc n)) 2))

(defn triangular-root
  [x]
  (->> (quadratic-root 1 1 (- (* 2 x)))
       (filter pos?)
       first
       int))

(defn nearest-triangular
  [goal n]
  (triangular-root (/ goal (triangular-number n))))

(defn count-rectangles
  [m n]
  (* (triangular-number m) (triangular-number n)))

(defn smallest-second [coll]
  (reduce #(min-key second %1 %2) coll))

(defn euler-085
  [goal]
  (->> (for [m (take-while #(< (triangular-number %) goal) (rest (range)))
             :let [n (nearest-triangular goal m)]
             :while (<= m n)]
         [(* m n) (- goal (count-rectangles m n))])
       smallest-second
       first))

(comment
  (time (euler-085 2000000))
  )
