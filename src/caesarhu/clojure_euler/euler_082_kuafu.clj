(ns caesarhu.clojure-euler.euler-082-kuafu
  (:require [clojure.string :as str]
            [caesarhu.kuafu.graph :as graph]))

(def test-data
  [[131  673 234 103 18]
   [201 96 342 965  150]
   [630  803  746  422  111]
   [537  699  497  121  956]
   [805  732  524   37  331]])

(def head 100000)
(def tail 100001)

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
  (let [m     (count matrix)
        n     (count (first matrix))
        pos (fn [[i j]]
              (+ (* i m) j))
        start-edges (for [i (range m)]
                      [:start [i 0] (get-in matrix [i 0])])
        end-edges (for [i (range n)]
                    [[i (dec n)] :end (get-in matrix [i (dec n)])])
        weight (fn [[i j]]
                 (get-in matrix [i j]))
        neighbor (fn [[i j]]
                   (remove nil? (list
                                 (when (< i (dec m)) [(inc i) j])
                                 (when (> i 0) [(dec i) j])
                                 (when (< j (dec n)) [i (inc j)]))))
        min-cost-flow (graph/min-cost-flow)
        supplies (->> (concat [1] (repeat (* m n) 0) [-1]))
        heads (->> (for [i (range m)]
                     (vector head (pos [i 0]) 100 (weight [i 0]))))
        tails (->> (for [i (range m)]
                     (vector (pos [i (dec n)]) tail 100 0)))]
    (->> (for [i (range m)
               j (range n)]
           (map vector (repeat [i j]) (neighbor [i j])))
         (apply concat)
         (map (fn [[start end]]
                (vector (pos start) (pos end) 100 (weight end))))
         (#(concat heads % tails))
         (mapv #(apply graph/add-arc-with-capacity-and-unitcost min-cost-flow %)))
    (mapv #(graph/set-node-supply min-cost-flow %1 %2) 
          (concat [head] (range (* m n)) [tail])
          supplies)
    (let [status (.solve min-cost-flow)]
      (if (= (str status) "OPTIMAL")
        (graph/get-optimal-cost min-cost-flow)
        (println "Solving the min cost flow problem failed. Solver status: " (str status))))))

(comment
  (time (euler-082-kuafu (get-data)))
  )