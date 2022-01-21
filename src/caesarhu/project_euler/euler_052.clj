(ns caesarhu.project-euler.euler-052
  (:require [caesarhu.math.math-tools :refer [digits]]))

(defn same-digits?
  [& xs]
  (let [ts (fn [n] (sort (digits n)))]
    (->> (map ts xs)
         (apply =))))

(defn permuted-multiples?
  [n]
  (apply same-digits? (map #(* n %) (range 1 7))))

(defn euler-052
  []
  (some #(and (permuted-multiples? %) %) (iterate inc 10)))

(comment
  (time (euler-052))
  )