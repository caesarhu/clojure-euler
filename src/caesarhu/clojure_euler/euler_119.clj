(ns caesarhu.clojure-euler.euler-119
  (:require [caesarhu.math.math-tools :refer [digits]]
            [clojure.math.numeric-tower :refer [expt]]
            [clojure.math.combinatorics :as combo]))

(defn digit-sum
  [n]
  (apply + (digits n)))

(defn power-seq
  [limit]
  (->> (for [i (range 2 limit)
             j (range 2 10)
             :let [power (expt i j)]
             :when (and (> power 10) (= i (digit-sum power)))]
         power)
       sort))

(defn euler-118
  [n]
  (nth (power-seq 100) (dec n)))

(comment
  (time (euler-118 30))
  )