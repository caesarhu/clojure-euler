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
        half (+ (quot length 2) (mod length 2))]
    (and (->> (for [i (range 2 (inc half))
                    :let [j (dec i)]]
                [(apply + (take i s))
                 (apply + (take-last j s))])
              (every? #(apply > %)))
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
       count))

(comment
  (euler-105)
  )
