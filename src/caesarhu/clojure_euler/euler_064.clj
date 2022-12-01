(ns caesarhu.clojure-euler.euler-064
  (:require [clojure.math.numeric-tower :as math]))

(defn continued-fraction [n]
  (let [[a0 r] (math/exact-integer-sqrt n)]
    (if (zero? r)
      [a0]
      (loop [m 0, d 1, a a0, acc [a0]]
        (if (= a (* 2 a0))
          acc
          (let [m (- (* d a) m), d (/ (- n (* m m)) d), a (quot (+ a0 m) d)]
            (recur m d a (conj acc a))))))))

(defn euler-064
  [limit]
  (->> (map continued-fraction (range 2 limit))
       (filter #(even? (count %)))
       count))

(comment
  (time (euler-064 10000))
  )