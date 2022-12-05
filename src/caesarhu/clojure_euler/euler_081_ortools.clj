(ns caesarhu.clojure-euler.euler-081-ortools
  (:require [clojure.string :as str])
  (:import [com.google.ortools Loader]
           [com.google.ortools.graph MinCostFlow MinCostFlowBase]))

(defn minimum-cost-flows
  []
  (let [min-cost-flow (MinCostFlow.)
        start-nodes [0, 0, 1, 1, 1, 2, 2, 3, 4]
        end-nodes [1, 2, 2, 3, 4, 3, 4, 4, 2]
        capacities [15, 8, 20, 4, 10, 15, 4, 20, 5]
        unit-costs [4, 4, 2, 2, 6, 1, 3, 2, 3]
        supplies [20, 0, 0, -5, -15]]
    (Loader/loadNativeLibraries)
    (doseq [i (range (count start-nodes))]
      (let [arc (.addArcWithCapacityAndUnitCost min-cost-flow (start-nodes i) (end-nodes i) (capacities i) (unit-costs i))]
        (when (not= arc i)
          (throw (Exception. "Internal error")))))
    (doseq [i (range (count supplies))]
      (.setNodeSupply min-cost-flow i (supplies i)))
    (let [status (.solve min-cost-flow)]
      (if (= (str status) "OPTIMAL")
        (do
          (println (str "Minimum cost: " (.getOptimalCost min-cost-flow)))
          (println)
          (println " Edge   Flow / Capacity  Cost")
          (doseq [i (range (.getNumArcs min-cost-flow))]
            (let [cost (* (.getFlow min-cost-flow i) (.getUnitCost min-cost-flow i))]
              (println (str (.getTail min-cost-flow i) " -> " (.getHead min-cost-flow i) "  " (.getFlow min-cost-flow i) "  / " (.getCapacity min-cost-flow i) "       " cost)))))
        (println "Solving the min cost flow problem failed. Solver status: " (str status))))))

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

(defn euler-081-ortools
  [matrix]
  (let [m     (count matrix)
        n     (count (first matrix))
        pos (fn [[i j]]
              (+ (* i m) j))
        weight (fn [[i j]]
                 (get-in matrix [i j]))
        neighbor (fn [[i j]]
                   (remove nil? (list
                                 (when (< i (dec m)) [(inc i) j])
                                 (when (< j (dec n)) [i (inc j)]))))
        min-cost-flow (MinCostFlow.)
        supplies (vec (concat [1] (repeat (- (* m n) 2) 0) [-1]))]
    (->> (for [i (range m)
               j (range n)]
           (map vector (repeat [i j]) (neighbor [i j])))
         (apply concat)
         (map (fn [[start end]]
                (vector (pos start) (pos end) 100 (weight end))))
         (mapv (fn [[start end capacity unit-cost]]
                 (.addArcWithCapacityAndUnitCost min-cost-flow start end capacity unit-cost))))
    (doseq [i (range m)
            j (range n)]
      (.setNodeSupply min-cost-flow (pos [i j]) (supplies (pos [i j]))))
    (let [status (.solve min-cost-flow)]
      (if (= (str status) "OPTIMAL")
        (+ (.getOptimalCost min-cost-flow) (weight [0 0]))
        (println "Solving the min cost flow problem failed. Solver status: " (str status))))))

(comment
  (time (euler-081-ortools (get-data)))
  (minimum-cost-flows)
  )