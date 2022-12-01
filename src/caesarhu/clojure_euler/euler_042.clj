(ns caesarhu.clojure-euler.euler-042
  (:require [caesarhu.math.math-tools :as tools]
            [clojure.math.numeric-tower :as math]))

(def fname "resources/p042_words.txt")

(defn get-data [fname]
  (sort (clojure.string/split (slurp fname) #",")))

(defn word-score [word]
  (reduce + (map #(- (int %) 64) (filter #(not= % \") word))))

(defn triangle-root [n]
  (/ (dec (math/sqrt (inc (* 8 n)))) 2))

(defn is-triangular? [n]
  (let [root (triangle-root n)]
    (== root (int root))))

(defn euler-042
  []
  (->> (get-data fname)
       (map word-score)
       (filter is-triangular?)
       count))

(comment
  (time (euler-042))
  )