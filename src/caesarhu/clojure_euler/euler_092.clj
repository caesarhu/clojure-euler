(ns caesarhu.clojure-euler.euler-092
  (:require [caesarhu.math.math-tools :refer [digits digits->number]]
            [clojure.math.combinatorics :as combo]))

(defn square-sum
  [s]
  (apply + (map #(* % %) s)))

(defn square-digit
  [n]
  (square-sum (digits n)))

(defn chain-result
  [n]
  (let [target #{0 1 89}
        x (if (int? n) n (digits->number n))]
    (loop [n x]
      (if-let [result (target n)]
        result
        (recur (square-digit n))))))

(defn brute-force
  [limit]
  (->> (range 1 limit)
       (remove #(= 1 (chain-result %)))
       count))

(defn happy-digits
  [count-of-digits]
  (->> (combo/combinations (mapcat #(repeat count-of-digits %) (range 10)) count-of-digits) 
       (filter #(= 1 (chain-result %)))
       (map combo/count-permutations)
       (apply +)))

(defn euler-092
  [limit]
  (let [count-of-digits (->> (digits limit) count dec)]
    (- limit 1 (happy-digits count-of-digits))))

(comment
  (time (euler-092 10000000))
  )