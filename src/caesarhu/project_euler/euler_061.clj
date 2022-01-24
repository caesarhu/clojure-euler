(ns caesarhu.project-euler.euler-061
  (:require [caesarhu.math.polynomial :as poly]))

(defn polygon
  ([a b c q]
   (let [f1 (poly/quadratic a b c)
         f2 #(quot % q)]
     (comp f2 f1)))
  ([a b c]
   (polygon a b c 1)))

(def polygons
  (map #(apply polygon %)
       [[1 1 0 2]
        [1 0 0]
        [3 -1 0 2]
        [2 -1 0]
        [5 -3 0 2]
        [3 -2 0]]))

(defn seperate
  [n]
  [(quot n 100) (mod n 100)])

(defn take-digits
  [f]
  (->> (map f (iterate inc 1))
       (drop-while #(< % 1000))
       (take-while #(< % 10000))))

(comment
  (take-digits p3))