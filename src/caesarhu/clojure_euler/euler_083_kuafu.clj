(ns caesarhu.clojure-euler.euler-083-kuafu
  (:require [clojure.string :as str]
            [caesarhu.kuafu.graph :as graph]))

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

(defn euler-083-kuafu
  [matrix]
  (let [length (count matrix)
        pos (fn [[i j]]
              (+ (* i length) j))
        weight (fn [[i j]]
                 (get-in matrix [i j]))
        neighbors (fn [[i j]]
                    (remove nil? (list
                                  (when (< i (dec length)) [(inc i) j])
                                  (when (< j (dec length)) [i (inc j)])
                                  (when (> i 0) [(dec i) j])
                                  (when (> j 0) [i (dec j)]))))
        start-node 100000
        nodes (map vector
                   (cons start-node (range (* length length)))
                   (concat [1] (repeat (dec (* length length)) 0) [-1]))
        matrix-arcs (for [i (range length)
                          j (range length)
                          arc (neighbors [i j])]
                      [(pos [i j]) (pos arc) 100 (weight arc)])
        all-arcs (cons [start-node (pos [0 0]) 100 (weight [0 0])] matrix-arcs)
        min-cost-flow (graph/min-cost-flow)]
    (doseq [arc all-arcs]
      (apply graph/add-arc-with-capacity-and-unitcost min-cost-flow arc))
    (doseq [node nodes]
      (apply graph/set-node-supply min-cost-flow node))
    (let [status (graph/solve min-cost-flow)]
      (if (= (str status) "OPTIMAL")
        (graph/get-optimal-cost min-cost-flow)
        (println "Solving the min cost flow problem failed. Solver status: " (str status))))))

(comment
  (euler-083-kuafu (get-data))
  )