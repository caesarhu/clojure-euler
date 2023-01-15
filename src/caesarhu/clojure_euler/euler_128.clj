(ns caesarhu.clojure-euler.euler-128
  (:require [caesarhu.math.primes :refer [is-prime?]]))

(defn rings-sum
  [n]
  (if (neg? n) 0
      (inc (*' n (inc n) 3))))

(defn level-bound
  [n]
  [(-> (dec n) rings-sum inc) (rings-sum n)])

(defn pd
  [n]
  (let [[head tail] (level-bound n)]
    (->> [(when (every? is-prime? [(- (* 6 n) 1) (+ (* 12 n) 5) (+ (* 6 n) 1)])
            head)
          (when (every? is-prime? [(- (* 6 n) 1) (- (* 12 n) 7) (+ (* 6 n) 5)])
            tail)]
         (remove nil?))))

(def pd3-seq
  (concat [1 2]
          (->> (iterate inc 2)
               (mapcat pd))))

(defn euler-128
  [target]
  (nth pd3-seq (dec target)))

(comment
  (take 10 pd3-seq)
  (time (euler-128 2000))
  )