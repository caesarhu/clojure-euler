(ns caesarhu.clojure-euler.euler-205
  (:require [clojure.math.combinatorics :as c]
            [clojure.math.numeric-tower :refer [expt round]]))

(defn dice-map
  [d n]
  (let [m (->> (c/selections (range 1 (inc d)) n)
               (map #(apply + %))
               frequencies)
        sum (->> (vals m) (apply +))]
    (->> (reduce (fn [m key]
                   (update m key / sum))
                 m
                 (keys m))
         (into (sorted-map)))))

(defn euler-205
  []
  (let [dice-66 (dice-map 6 6)
        dice-49 (dice-map 4 9)]
    (->> (for [k66 (->> (keys dice-66) sort)]
           (let [p66 (dice-66 k66)
                 win-p (->> (filter (fn [[k49 p49]] (> k49 k66)) dice-49)
                            (map last)
                            (apply +))]
             (* p66 win-p)))
         (apply +)
         (* (expt 10 7))
         round
         (* (expt 10 -7))
         double)))

(comment
  (euler-205)
  )
