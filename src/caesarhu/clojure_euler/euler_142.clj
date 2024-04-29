(ns caesarhu.clojure-euler.euler-142
  (:require [clojure.math.combinatorics :as combo]
            [caesarhu.math.math-tools :refer [square? gcd* pythagorean-triplet]]
            [clojure.math.numeric-tower :refer [sqrt]]
            [caesarhu.math.primes :as p]
            [clojure.set :as set]))

(def bound 2500)

(defn square
  [^long n]
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

(defn find-xyz
  [[a b c]]
  (let [[a2 b2 c2] (map square [a b c])]
    (for [i (range (+ (mod c 2) 2) (quot c 2) 2)
          :let [i2 (square i)
                x (quot (+ c2 i2) 2)
                y (quot (- c2 i2) 2)
                z (- b2 x)]
          :when (and (square? (- x z))
                     (square? (+ y z)))]
      [x y z])))

(defn triplets
  [^long limit]
  (let [ts (->> (pythagorean-triplet #(<= (apply + %) limit))
                (map sort))
        times (fn [s]
                (->> (iterate inc 1)
                     (map (fn [n] (map #(* % n) s)))
                     (take-while #(<= (apply + %) limit))))]
    (mapcat times ts)))

(defn euler-142
  []
  (->> (triplets bound)
       (mapcat find-xyz)
       (map #(apply + %))
       (apply min)))

(comment
  (time (triplets 2500))
  (time (euler-142))
  )
