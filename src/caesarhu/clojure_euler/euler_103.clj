(ns caesarhu.clojure-euler.euler-103
  (:require [clojure.math.combinatorics :as combo]))

(defn distinct-sum?
  [v]
  (apply distinct? (map #(apply + %) v)))

(defn special-set?
  [s]
  (let [length (count s)
        sorted (sort s)]
    (and (->> (for [i (range 2 (inc (/ length 2)))
                    :let [j (dec i)]]
                (> (apply + (take i sorted))
                   (apply + (take-last j sorted))))
              (every? true?))
         (->> (for [i (range 2 (inc (quot length 2)))]
                (combo/combinations s i))
              (every? distinct-sum?)))))

(defn next-set
  [s]
  (let [base (nth s (quot (count s) 2))]
    (->> (range (+ base (first s)) (+ base (last s) 2))
         (#(combo/combinations % (count s)))
         (map #(cons base %))
         (filter special-set?)
         (apply min-key #(apply + %)))))

(defn euler-103
  [n]
  (loop [result [1]]
    (if (= n (count result))
      result
      (recur (next-set result)))))

(comment
  (time (euler-103 8))
  )