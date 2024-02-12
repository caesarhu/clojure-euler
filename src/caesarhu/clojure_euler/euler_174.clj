(ns caesarhu.clojure-euler.euler-174
  (:require [clojure.math.numeric-tower :refer [exact-integer-sqrt]]))

(defn diff-square
  [a b]
  (-> (* (+ a b) (- a b)) abs))

(defn get-laminaes
  [limit n]
  (->> (iterate #(+ 2 %) (+ n 2))
       (map #(diff-square n %))
       (take-while #(<= % limit))))

(defn euler-174
  [limit]
  (->> (iterate inc 1)
       (map #(get-laminaes limit %))
       (take-while not-empty)
       (apply concat)
       frequencies
       vals
       frequencies
       (filter (fn [[k v]] (<= k 10)))
       (map last)
       (apply +)))

(comment
  (time (euler-174 1000000))
  )
