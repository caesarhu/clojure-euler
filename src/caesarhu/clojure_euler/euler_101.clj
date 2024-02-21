(ns caesarhu.clojure-euler.euler-101
  (:require [clojure.math.numeric-tower :as math]))

(defn gf
  [x]
  (->> (range 11)
       (map (fn [n]
              (* (math/expt -1 n) (math/expt x n))))
       (apply +)))

(defn neville [xs ys n x]
  ((memoize (fn p [i j]
              (if (= i j) (ys i)
                  (/ (- (* (- x (xs j)) (p i (dec j)))
                        (* (- x (xs i)) (p (inc i) j)))
                     (- (xs i) (xs j))))))
   0 n))

(defn get-fits []
  (for [k (range 1 11)]
    (let [xs (vec (range 1 (inc k)))
          ys (vec (map gf xs))
          nev #(neville xs ys (dec k) %)
          fit-ind (first (filter #(not= (gf %) (nev %))
                                 (iterate inc 1)))]
      (nev fit-ind))))

(defn euler-101
  []
  (apply + (get-fits)))

(comment
  (neville [4 5 6] [10 21/4 1] 2 18)
  (time (euler-101))
  )
