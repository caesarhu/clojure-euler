(ns caesarhu.clojure-euler.euler-147
  (:require [clojure.math.combinatorics :refer [count-combinations]]))

(defn sub-rectangles
  [m n]
  (* (count-combinations (range (inc m)) 2)
     (count-combinations (range (inc n)) 2)))

(defn sub-diagonal-rectangles
  [m n]
  (let [y (max m n)
        x (min m n)]
    (+ (/ (* (dec x) x (+ 3 (* 4 x) (* 4 x x))) 6)
       (/ (* (- y x) x (dec (* 4 x x))) 3))))

(defn euler-147
  [m n]
  (apply + (for [x (range 1 (inc m))
                 y (range 1 (inc n))]
             (+ (sub-rectangles x y)
                (sub-diagonal-rectangles x y)))))

(comment
  (time (euler-147 47 43))
  )
