(ns caesarhu.clojure-euler.euler-173
  (:require [clojure.math.numeric-tower :refer [expt]]))

(defn diff-square
  [a b]
  (-> (* (+ a b) (- a b)) abs))

(defn count-laminaes
  [limit n]
  (->> (iterate #(+ 2 %) (+ n 2))
       (map #(diff-square n %))
       (take-while #(<= % limit))
       count))

(defn euler-173
  [limit]
  (->> (iterate inc 1)
       (map #(count-laminaes limit %))
       (take-while pos-int?)
       (apply +)))

(comment
  (time (euler-173 1000000))
  )
