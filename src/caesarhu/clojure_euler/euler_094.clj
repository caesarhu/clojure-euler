(ns caesarhu.clojure-euler.euler-094
  (:require [clojure.math.numeric-tower :refer [exact-integer-sqrt]]
            [clojure.set :as set]
            [caesarhu.math.polynomial :refer [quadratic-root]]))

(defn square?
  [n]
  (and (int? n) (zero? (last (exact-integer-sqrt n)))))

(defn euclid-formula
  [m n]
  (let [mm (* m m)
        nn (* n n)]
    (sort [(- mm nn) (* 2 m n) (+ mm nn)])))

(defn formula-1
  [m]
  (let [x2 (/ (- (* m m) 1) 3)
        y2 (/ (+ (* m m) 1) 3)]
    (cond
      (square? x2) (first (exact-integer-sqrt x2))
      (square? y2) (first (exact-integer-sqrt y2)))))

(defn formula-2
  [m]
  (let [m2 (* m m)
        f1 [1 (* -1 4 m) (- m2 1)]
        f2 [1 (* -1 4 m) (+ m2 1)]]
    (some->> (set/union (apply quadratic-root f1) (apply quadratic-root f2))
             (filter int?)
             not-empty
             (apply min))))

(defn formula
  [m]
  (or (formula-1 m) (formula-2 m)))

(defn take-triplet
  [limit]
  (->> (iterate inc 2)
       (map #(vector % (formula %)))
       (filter #(int? (last %)))
       (map (fn [[m n]]
              (let [[a b c] (euclid-formula m n)
                    triplet [c c (* 2 a)]]
                [(apply + triplet) triplet])))
       (take-while #(<= (first %) limit))))

(defn euler-094
  [limit]
  (->> (take-triplet limit)
       (map first)
       (apply +)))

(comment
  (time (take-triplet 1000000000))
  (time (euler-094 1000000000))
  )