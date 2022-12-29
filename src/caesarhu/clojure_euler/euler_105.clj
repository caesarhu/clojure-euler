(ns caesarhu.clojure-euler.euler-105
  (:require [clojure.math.combinatorics :as combo]
            [clojure.string :as str]))

(def file-105 "resources/p105_sets.txt")

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

(defn euler-105
  []
  (->> (slurp file-105)
       str/split-lines
       (map #(str/split % #","))
       (map (fn [v]
              (map #(Long/parseLong %) v)))
       (filter special-set?)
       (map #(apply + %))
       (apply +)))

(comment
  (time (euler-105))
  )
