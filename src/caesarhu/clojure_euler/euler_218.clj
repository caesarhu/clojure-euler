(ns caesarhu.clojure-euler.euler-218
  (:require [clojure.math.numeric-tower :refer [expt gcd sqrt]]
            [caesarhu.math.math-tools :refer [pythagorean-mn square?]]))

(defn pythagorean-triplet
  [limit]
  (->> (map pythagorean-mn (iterate inc 2))
       (map (fn [v] (take-while #(<= (last %) limit) v)))
       (take-while not-empty)
       (apply concat)))

(defn not-super-perfect?
  [v]
  (let [[a b c] v
        x (quot (*' a b) 2)]
    (and (square? c) (pos-int? (mod x 6)) (pos-int? (mod x 28)))))

(defn euler-218
  [limit]
  (let [target (expt limit 1/2)
        triplets (pythagorean-triplet target)
        sum (->> (filter not-super-perfect? triplets) count)
        make-perfect (fn [v]
                       (let [[a b c] v]
                         [(- (* b b) (* a a)) (* 2 a b) (* c c)]))]
    (->> (map make-perfect triplets)
         (filter not-super-perfect?)
         count
         (+ sum))))

(comment
  (time (euler-218 (expt 10 16)))
  )
