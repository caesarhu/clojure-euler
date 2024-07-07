(ns caesarhu.clojure-euler.euler-030
  (:require [clojure.math.numeric-tower :refer [expt]]
            [caesarhu.math.math-tools :refer [digits]]))

(defn sum-powers-5
  [n]
  (->> (map #(expt % 5) (digits n))
       (apply +)))

(defn powers-5-number?
  [n]
  (= n (sum-powers-5 n)))

(defn euler-030
  []
  (->> (range 2 (* 6 (expt 9 5)))
       (filter powers-5-number?)
       (apply +)))

(comment
  (time (euler-030))
  )
