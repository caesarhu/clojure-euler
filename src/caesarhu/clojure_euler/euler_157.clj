(ns caesarhu.clojure-euler.euler-157
  (:require [caesarhu.math.primes :as p]
            [caesarhu.math.math-tools :refer [coprime?]]
            [clojure.math.combinatorics :as combo]
            [clojure.math.numeric-tower :refer [expt]]))

(defn basic-pairs
  [n]
  (filter #(apply coprime? %)
          (combo/combinations (->> (p/divisors (expt 10 n))
                                   (repeat 2)
                                   (apply concat))
                              2)))

(defn get-p
  [n a b]
  (* (expt 10 n) (+ (/ a) (/ b))))

(defn get-n
  [n]
  (->> (map #(apply get-p n %) (basic-pairs n))
       (map p/divisors)
       (map count)
       (apply +)))

(defn euler-157
  [n]
  (->> (range 1 (inc n))
       (map get-n)
       (apply +)))

(comment
  (time (euler-157 9))
  )
