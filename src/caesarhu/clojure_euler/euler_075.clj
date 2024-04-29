(ns caesarhu.clojure-euler.euler-075
  (:require [caesarhu.math.math-tools :refer [pythagorean-triplet]]))

(defn euler-075
  [^long limit]
  (let [triplets (pythagorean-triplet #(<= (apply + %) limit))
        times (fn [n] (take-while #(<= % limit) (iterate (partial + n) n)))]
    (->> triplets
         (map #(apply + %))
         (mapcat times)
         frequencies
         (filter #(= 1 (val %)))
         count)))

(comment
  (time (euler-075 1500000))
  )
