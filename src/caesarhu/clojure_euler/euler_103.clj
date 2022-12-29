(ns caesarhu.clojure-euler.euler-103
  (:require [clojure.math.combinatorics :as combo]
            [clojure.set :as set]))

(defn distinct-sum?
  [v]
  (apply distinct? (map #(apply + %) v)))

(defn special-set?
  [s]
  (let [length (count s)
        half (+ (quot length 2) (mod length 2))]
    (and (->> (for [i (range 2 (inc half))
                    :let [j (dec i)]]
                [(apply + (take i s)) 
                 (apply + (take-last j s))])
              (every? #(apply > %)))
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
  (time (euler-103 7))
  )