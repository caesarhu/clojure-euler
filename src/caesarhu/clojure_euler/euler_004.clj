(ns caesarhu.clojure-euler.euler-004
  (:require [caesarhu.math.math-tools :refer [palindrome?]]))

(defn euler-004
  []
  (let [seq-11 (->> (range 11 1000 11) (drop-while #(< % 100)) (take-while #(< % 1000)))]
    (->> (range 999 900 -1)
         (mapcat (fn [n] (map #(* n %) seq-11)))
         (into (sorted-set-by >))
         (filter palindrome?)
         first)))

(comment
  (time (euler-004))
  )
