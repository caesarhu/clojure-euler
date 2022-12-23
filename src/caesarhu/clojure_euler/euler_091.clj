(ns caesarhu.clojure-euler.euler-091
  (:require [clojure.math.combinatorics :as combo]
            [clojure.math.numeric-tower :refer [gcd]]))

(defn square
  [n]
  (* n n))

(defn square-sum
  [pos]
  (apply + (map square pos)))

(defn length-square
  [pos1 pos2]
  (square-sum (map - pos1 pos2)))

(defn right-angle?
  [pos1 pos2]
  (let [triplet (->> (combo/combinations [pos1 pos2 [0 0]] 2)
                     (map #(apply length-square %))
                     sort)]
    (= (last triplet) (apply + (take 2 triplet)))))

(defn brute-force
  [limit]
  (let [space (range (inc limit))
        pos-seq (rest (combo/cartesian-product space space))]
    (->> (for [p pos-seq
               q pos-seq
               :when (not= p q)]
           (set [p q]))
         set
         (filter #(apply right-angle? %))
         count)))

(defn euler-091
  [limit]
  (let [c (* 3 (* limit limit))]
    (reduce + c (for [x1 (range 1 (inc limit))
                      y1 (range 1 (inc limit))
                      :let [p (gcd x1 y1)]]
                  (* 2 (min (quot (* p x1) y1) (quot (* p (- limit y1)) x1)))))))

(comment
  (time (brute-force 50))
  (time (euler-091 50))
  )