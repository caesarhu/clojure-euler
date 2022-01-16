(ns caesarhu.project-euler.euler-004
  (:require [caesarhu.math.math-tools :refer [palindrome?]]))

(defn euler-004
  []
  (let [d3-seq (range 901 1000)]
    (->> (for [i d3-seq
               j d3-seq]
           (* i j))
         sort
         reverse
         (some #(and (palindrome? %) %)))))

(comment
  (time (euler-004))
  )