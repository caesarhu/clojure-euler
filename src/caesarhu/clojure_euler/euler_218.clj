(ns caesarhu.clojure-euler.euler-218
  (:require [clojure.math.numeric-tower :refer [expt]]
            [caesarhu.math.math-tools :refer [next-pythagorean-triplet square?]]))

(defn super-perfect?
  [[^long a, ^long b]]
  (-> (*' a b) (mod 168) zero?))

(defn generate-perfect
  [a b]
  [(abs (- (* a a) (* b b)))
   (* 2 a b)])

(defn euler-218
  [^long limit]
  (loop [base (list [3 4 5])
         sum 0]
    (if (empty? base)
      sum
      (let [new-triangles (->> (next-pythagorean-triplet (first base))
                               (filter (fn [s]
                                         (let [c (last s)
                                               c2 (* c c)]
                                           (<= c2 limit)))))
            count-not-super (->> (map #(apply generate-perfect (take 2 %)) new-triangles)
                                 (remove super-perfect?)
                                 count)]
        (recur (apply conj (rest base) new-triangles)
               (+ sum count-not-super))))))

(comment
  (time (euler-218 (expt 10 16)))
  )
