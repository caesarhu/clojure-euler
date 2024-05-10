(ns caesarhu.clojure-euler.euler-124
  (:require [caesarhu.math.math-tools :refer [factors-range]]))

(defn rad
  [m]
  (apply * (keys m)))

(defn euler-124
  [limit target]
  (nth (->> (factors-range limit)
            (map rad)
            (map vector (range))
            (drop 1)
            (sort-by last)) (- target 2)))

(comment
  (time (euler-124 100000 10000))
  )
