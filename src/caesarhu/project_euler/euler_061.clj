(ns caesarhu.project-euler.euler-061
  (:require [caesarhu.math.polynomial :as poly]))

(defn p3
  [n]
  (let [f (poly/quadratic 1 1 0)]
    (quot (f n) 2)))

(defn p4
  [n]
  (* n n))

(defn p5
  [n]
  (let [f (poly/quadratic 3 -1 0)]
    (quot (f n) 2)))

(defn p6
  [n]
  (let [f (poly/quadratic 2 -1 0)]
    (f n)))

(defn p7
  [n]
  ((let [f (poly/quadratic 5 -3 0)]
     (quot (f n) 2))))

(defn p6
  [n]
  (let [f (poly/quadratic 3 -2 0)]
    (f n)))

(comment
  (p6 3)
  )