(ns caesarhu.clojure-euler.euler-107
  (:require [clojure.string :refer [split-lines split]]
            [loom.graph :as graph]
            [loom.alg :as alg]))

(def network-40 "resources/p107_network.txt")

(defn get-adjacency-matrix [fname]
  (let [raw-data (split-lines (slurp fname))
        parse-csv (fn [x] (vec (map read-string (split x #","))))]
    (vec (map parse-csv raw-data))))

(defn ->loom-weighted-graph
  [matrix]
  (->> (for [i (range (count matrix))]
         (->> (for [j (range (count (nth matrix i)))
                    :let [weight (get-in matrix [i j])]
                    :when (int? weight)
                    :let [char-i (char (+ i 65))
                          char-j (char (+ j 65))]]
                {(keyword (str char-i))
                 {(keyword (str char-j))
                  weight}})
              (apply merge-with merge)))
       (apply merge-with merge)
       (graph/weighted-graph)))

(defn weight
  [wg]
  (->> (let [wm (:adj wg)]
         (for [[_ m] wm
               [_ w] m]
           w))
       (apply +)
       (#(/ % 2))))

(defn euler-107
  [fname]
  (let [origin (-> (get-adjacency-matrix fname)
                   (->loom-weighted-graph))]
    (- (weight origin)
       (weight (alg/prim-mst origin)))))

(comment
  (time (euler-107 network-40))
  )