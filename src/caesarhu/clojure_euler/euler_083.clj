(ns caesarhu.clojure-euler.euler-083
  (:require [clojure.string :as str]
            [loom.graph :as g]
            [loom.alg :as alg]
            [clojure.math.numeric-tower :as math]))

(def test-data
  [[131 673 234 103  18]
   [201  96 342 965 150]
   [630 803 746 422 111]
   [537 699 497 121 956]
   [805 732 524  37 331]])

(def fname "resources/p083_matrix.txt")

(defn get-data
  []
  (->> (slurp fname)
       (str/split-lines)
       (map #(str/split % #","))
       (map (fn [coll]
              (map #(Long/parseLong %) coll)))
       (map vec)
       vec))

(defn ->weight-digraph
  [matrix]
  (let [length     (count matrix)
        weight (fn [[i j]]
                 (get-in matrix [i j]))
        neighbors (fn [[i j]]
                    (remove nil? (list
                                  (when (< i (dec length)) [(inc i) j])
                                  (when (< j (dec length)) [i (inc j)])
                                  (when (> i 0) [(dec i) j])
                                  (when (> j 0) [i (dec j)]))))]
    (->> (for [i (range length)
               j (range length)
               neighbor (neighbors [i j])]
           [[i j] neighbor (weight neighbor)])
         (cons [[100000 100000] [0 0] (weight [0 0])])
         (apply g/weighted-digraph))))

(defn euler-083
  [matrix]
  (let [wdg (->weight-digraph matrix)]
    (-> (alg/dijkstra-path-dist wdg [100000 100000] (repeat 2 (dec (count matrix))))
        last)))

(comment
  (time (euler-083 (get-data)))
  )