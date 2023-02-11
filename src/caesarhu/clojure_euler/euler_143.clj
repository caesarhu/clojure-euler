(ns caesarhu.clojure-euler.euler-143
  (:require [clojure.math.numeric-tower :refer [gcd sqrt]]
            [clojure.set :as set]
            [clojure.math.combinatorics :refer [combinations]]
            [clojure.math.combinatorics :as comb]))

(defn generate-triangle
  [m n]
  (let [a (+ (* 2 m n) (* n n))
        b (- (* m m) (* n n))
        c (+ (* m m) (* n n) (* m n))]
    [(min a b) (max a b) c]))

(defn generate-map
  [limit]
  (->> (for [m (range 2 limit)
             n (range 1 m)
             :when (and (= 1 (gcd m n))
                        (pos? (mod (- m n) 3)))
             :let [[a b c] (generate-triangle m n)]
             :while (<= c limit)]
         (for [i (iterate inc 1)
               :let [[x y z] (map #(* i %) [a b c])]
               :while (<= z limit)]
           {x (sorted-set y)}))
       (apply concat)
       (apply merge-with set/union)
       (remove #(< (count (last %)) 2))
       (into {})))

(defn euler-143
  [limit]
  (let [pqr-map (generate-map limit)]
    (->> (for [[p qs] pqr-map
               q qs
               :let [rs (pqr-map q)]
               r (set/intersection rs qs)
               :let [sum (+ p q r)]
               :when (and (> r q)
                          (<= sum limit))]
           sum)
         set
         (apply +))))

(comment
  (time (euler-143 120000))
  )