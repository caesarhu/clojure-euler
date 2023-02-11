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
                         (map #(hash-map (last %) [(take 2 %)]))
                         (apply merge-with concat))]
    (for [[f v] triplet-map
          :let [ds (->> v (apply concat) set)]
          [a e] v
          [b d] (triplet-map e)
          :when (and (ds d) (> d a) (even? (- (square d) (square a))))]
      [a b (sqrt (+ (square a) (square b))) d e f])))

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
  (time (euler-142 2500))
  )