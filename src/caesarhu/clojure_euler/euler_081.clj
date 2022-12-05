(ns caesarhu.clojure-euler.euler-081
  (:require [clojure.string :as str]
            [loom.graph :as g]
            [loom.alg :as alg]))

(def test-data
  [[131,673,234,103,18]
   [201,96,342,965,150]
   [630,803,746,422,111]
   [537,699,497,121,956]
   [805,732,524,37,331]])

(defn get-data
  []
  (->> (slurp "resources/p081_matrix.txt")
       (str/split-lines)
       (map #(str/split % #","))
       (map (fn [coll]
              (map #(Long/parseLong %) coll)))
       (map vec)
       vec))

(defn ->weight-digraph
  [matrix]
  (let [m     (count matrix)
        n     (count (first matrix))
        weight (fn [[i j]]
                 [[i j] (get-in matrix [i j])])
        neighbor (fn [[i j]]
                   (remove nil? (list
                                 (when (< i (dec m)) [(inc i) j])
                                 (when (< j (dec n)) [i (inc j)]))))
        add-weight (fn [[pos1 w1] [pos2 w2]]
                     [pos1 pos2  (+ w1 w2)])]
    (->> (for [i (range m)
               j (range n)
               :let [node-weight (weight [i j])
                     neighbor-weight (map weight (neighbor [i j]))]]
           (map #(add-weight node-weight %) neighbor-weight))
         (apply concat)
         (apply g/weighted-digraph))))

(defn euler-081
  [matrix]
  (let [edge (count matrix)
        weight-addition (+ (get-in matrix [0 0]) (get-in matrix [(dec edge) (dec edge)]))
        wdg (->weight-digraph matrix)]
    (->> (alg/shortest-path wdg [0 0] [(dec edge) (dec edge)])
         (map #(get-in matrix %))
         (apply +))))

(comment
  (time (euler-081 test-data))
  )

