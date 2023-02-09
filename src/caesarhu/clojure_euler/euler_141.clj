(ns caesarhu.clojure-euler.euler-141
  (:require [clojure.math.numeric-tower :refer [expt]]
            [caesarhu.math.math-tools :refer [square? coprime?]]))

(defn generate-square
  [a b c]
  (+' (*' a a a b c c)
      (*' b b c)))

(defn euler-141
  [limit]
  (let [n-limit (first (drop-while #(< (* % % %) limit) (iterate inc 2)))]
    (->> (for [a (range 2 n-limit)
               b (range 1 a)
               :when (and (coprime? a b)
                          (< (+' (*' a a a b b) (*' b b)) limit))
               c (take-while #(< (generate-square a b %) limit) (iterate inc 1))
               :let [n (generate-square a b c)]
               :when (square? n)]
           n)
         set
         (apply +))))

(comment
  (time (euler-141 (expt 10 12)))
  )