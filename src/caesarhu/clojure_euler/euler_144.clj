(ns caesarhu.clojure-euler.euler-144
  (:require [clojure.math.numeric-tower :refer [sqrt]]))

(defn next-point
  [point-0 point-1]
  (let [[x0 y0] point-0
        [x1 y1] point-1
        m (/ (- y0 y1) (- x0 x1))
        normal (/ y1 (* 4 x1))
        n (/ (- (* 2 normal) (* m (- 1 (* normal normal)))) (+ 1 (- (* normal normal)) (* 2 normal m)))
        x2 (/ (- (* n n x1) (* 4 x1) (* 2 n y1)) (+ 4 (* n n)))
        y2 (+ y1 (* n (- x2 x1)))]
    [point-1 [x2 y2]]))

(defn euler-144
  [point-0 point-1]
  (let [bound-x 0.01
        escape? (fn [[x y]] (and (<= (abs x) bound-x) (pos? y)))]
    (->> (iterate #(apply next-point %) [point-0 point-1])
         rest
         (take-while #(not (escape? (first %))))
         count)))

(comment
  (<= 0.016527799669878617 0.01)
  (euler-144 [0.0,10.1] [1.4,-9.6])
  )