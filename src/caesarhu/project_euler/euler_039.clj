(ns caesarhu.project-euler.euler-039
  (:require [clojure.math.numeric-tower :as math]))

(defn coprime?
  [a b]
  (= 1 (math/gcd a b)))

(defn euler-039
  [limit]
  (->> (for [m (range 2 (math/sqrt (/ limit 2)))
             n (range (if (even? m) 1 2) m 2) :when (coprime? m n)
             :let [p (* 2 m (+ m n))] :when (<= p limit)
             i (range 1 (/ limit p))]
         (* p i))
       frequencies
       (apply max-key val)
       first))

(comment
  (time (euler-039 1000))
  )