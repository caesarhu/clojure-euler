(ns caesarhu.clojure-euler.euler-180
  (:require [clojure.math.numeric-tower :refer [expt sqrt]]
            [clojure.math.combinatorics :as c]))

(defn square [n] (* n n))

(defn xy-fn
  [[x y]]
  [(+ x y)
   (/ 1 (+ (/ 1 x) (/ 1 y)))
   (sqrt (+ (square x) (square y)))
   (sqrt (/ 1 (+ (/ 1 (square x)) (/ 1 (square y)))))])

(defn k-set
  [k]
  (set (for [a (range 1 k)
             b (range (inc a) (inc k))]
         (/ a b))))

(defn get-xyz
  [k]
  (let [ks (k-set k)
        xyz (fn [xy]
              (for [z (xy-fn xy)
                    :when (ks z)]
                (conj xy z)))]
    (->> (c/cartesian-product ks ks)
         (mapcat xyz))))

(defn euler-180
  [k]
  (let [sum (->> (get-xyz k)
                 (map #(apply +' %))
                 set
                 (apply +'))]
    (->> (apply +' ((juxt numerator denominator) sum))
         long)))

(comment
  (count (get-xyz 35))
  (time (euler-180 35))
  )
