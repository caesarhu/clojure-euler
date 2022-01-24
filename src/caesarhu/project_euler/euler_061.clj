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

(defn p8
  [n]
  (let [f (poly/quadratic 3 -2 0)]
    (f n)))

(defn seperate
  [n]
  [(quot n 100) (mod n 100)])

(comment
  (->> (map p8 (iterate inc 1))
       (take 100))
  )