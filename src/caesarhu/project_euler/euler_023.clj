(ns caesarhu.project-euler.euler-023
  (:require [caesarhu.project-euler.euler-021 :refer [proper-divisors-sum]]
            [clojure.math.combinatorics :as combo]))

(defn abundant?
  [n]
  (> (proper-divisors-sum n) n))

(def bound 28123)

(def abundant-numbers
  (filter abundant? (range 1 bound)))

(def abundant-set
  (set abundant-numbers))

(defn abundant-sum?
  [n]
  (some #(abundant-set (- n %)) (take-while #(<= % (inc (quot n 2))) abundant-numbers)))

(defn euler-023
  []
  (->> (filter #(not (abundant-sum? %)) (range 1 (inc bound)))
       (apply +)))

(comment
  (time (euler-023))
  )