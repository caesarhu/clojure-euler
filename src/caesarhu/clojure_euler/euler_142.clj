(ns caesarhu.clojure-euler.euler-142
  (:require [clojure.math.combinatorics :as combo]
            [caesarhu.math.math-tools :refer [square? gcd* pythagorean-triplet]]
            [clojure.math.numeric-tower :refer [sqrt]]
            [caesarhu.math.primes :as p]
            [clojure.set :as set]))

(defn square
  [n]
  (* n n))

(defn pair?
  [a b]
  (and (square? (+ a b))
       (square? (abs (- a b)))))

(defn generate-square
  [sum]
  (->> (p/divisors sum)
       (take-while #(< % (sqrt sum)))
       (map #(vector % (quot sum %)))
       (filter #(even? (apply + %)))
       (map (fn [[m n]]
              (let [x (/ (+ m n) 2)]
                [(- x m) x])))))

(defn xyz
  [[a b]]
  (let [[sa sb] (->> [a b] (map square) sort)
        z (/ (- sb sa) 2)
        y (+ z sa)]
    (->> (generate-square (+ sa sb))
         (map #(+ y (square (first %))))
         (filter #(pair? % z))
         (map #(vector % y z)))))

(defn brute-force
  [limit]
  (->> (iterate #(+ % 2) 2)
       (take limit)
       (#(combo/combinations % 2))
       (mapcat xyz)
       first))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn pythagorean
  [limit]
  (->> (pythagorean-triplet limit)
       (mapcat (fn [v]
              (->> (iterate inc 1)
                   (map #(map (partial * %) v))
                   (take-while #(< (apply + %) limit)))))))

(defn match-squares
  [limit]
  (let [triplet-map (->> (pythagorean limit)
                         (map #(hash-map (last %) [%]))
                         (apply merge-with concat))]
    (->> (for [[f v] triplet-map]
           (let [ds (->> (mapcat #(take 2 %) v) set)]
             (for [e (seq ds)
                   :when (triplet-map e)
                   te (triplet-map e)
                   d (take 2 te)
                   :when (ds d)
                   :let [c (sqrt (- (square f) (square d)))]
                   :when (triplet-map c)
                   tc (triplet-map c)
                   :let [b (first (set/intersection (set te) (set tc)))]
                   :when b
                   :let [a (sqrt (- (square c) (square b)))]
                   :when (and (> d a)
                              (even? (- (square d) (square a))))]
               [a b c d e f])))
         (apply concat))))

(defn euler-142
  [limit]
  (->> (match-squares limit)
       (map (fn [[a b c d e f]]
              (let [z (/ (- (square d) (square a)) 2)
                    y (+ z (square a))
                    x (+ y (square b))]
                [(+ x y z) [x y z] [a b c d e f]])))
       sort))

(comment
  (time (euler-142 10000))
  )