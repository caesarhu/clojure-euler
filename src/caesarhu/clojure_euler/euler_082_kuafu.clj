(ns caesarhu.clojure-euler.euler-082-kuafu
  (:require [clojure.string :as str]
            [caesarhu.kuafu.graph :as graph]))

(def test-data
  [[131  673 234 103 18]
   [201 96 342 965  150]
   [630  803  746  422  111]
   [537  699  497  121  956]
   [805  732  524   37  331]])

(defn get-data
  []
  (->> (slurp "resources/p082_matrix.txt")
       (str/split-lines)
       (map #(str/split % #","))
       (map (fn [coll]
              (map #(Long/parseLong %) coll)))
       (map vec)
       vec))

(defn euler-082-kuafu
  [matrix]
  (let [length (count matrix)
        pos (fn [[i j]]
              (+ (* i length) j 1))
        weight (fn [[i j]]
                 (get-in matrix [i j]))
        neighbors (fn [[i j]]
                    (remove nil? (list
                                  (when (< i (dec length)) [(inc i) j])
                                  (when (> i 0) [(dec i) j])
                                  (when (< j (dec length)) [i (inc j)]))))
        start-node 0
        target-node (inc (* length length))
        nodes (map vector
                   (concat [start-node] (range 1 target-node) [target-node])
                   (concat [1] (repeat (* length length) 0) [-1]))
        start-arcs (->> (range length)
                        (map #(vector % 0))
                        (map #(vector start-node (pos %) 100 (weight %))))
        target-arcs (->> (range length)
                         (map #(vector % (dec length)))
                         (map #(vector (pos %) target-node 100 0)))
        matrix-arcs (for [i (range length)
                          j (range length)
                          arc (neighbors [i j])]
                      [(pos [i j]) (pos arc) 100 (weight arc)])
        all-arcs (concat start-arcs matrix-arcs target-arcs)
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
  (time (euler-082-kuafu (get-data)))
  )