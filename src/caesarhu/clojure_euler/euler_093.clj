(ns caesarhu.clojure-euler.euler-093
  (:require [clojure.math.combinatorics :as combo]
            [clojure.set :as set]))

(defn count-consecutive
  [s]
  (loop [[x & xs] s
         result 0]
    (if (= x (inc result))
      (recur xs x)
      result)))

(defn arithmetics
  [n1 n2]
  (let [operators (if (zero? n2) [+ - *] [+ - * /])]
    (map #(% n1 n2) operators)))

(defn compute-vector
  [v]
  (->> (reduce (fn [acc n]
                 (mapcat #(arithmetics % n) acc))
               [(first v)]
               (rest v))
       (filter integer?)
       (map abs)
       (apply sorted-set)
       (#(disj % 0))))

(defn comopute-permuted
  [v]
  (->> (combo/permutations v)
       (map compute-vector)
       (apply set/union)
       count-consecutive))

(defn euler-093
  [n]
  (->> (combo/combinations (range 1 10) n)
       (apply max-key comopute-permuted)))

(comment
  (time (euler-093 4))
  )