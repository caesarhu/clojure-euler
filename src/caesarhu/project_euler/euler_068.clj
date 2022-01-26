(ns caesarhu.project-euler.euler-068
  (:require [clojure.math.combinatorics :as combo]))

(defn gon-edges
  [n]
  (let [inner (->> (concat (range n) [0])
                   (partition 2 1))
        outer (range n (* n 2))]
    (map #(cons %2 %1) inner outer)))

(defn euler-068
  [n]
  (let [edges (gon-edges n)
        ->edges (fn [v] (map #(map v %) edges))
        magic? (fn [v]
                 (->> (map #(apply + %) v) (apply =)))
        init-outer (->> (range (inc n) (inc (* n 2)))
                        reverse
                        (#(cons (last %) (butlast %))))]
    (->> (range 1 (inc n))
         reverse
         combo/permutations
         (map #(vec (concat % init-outer)))
         (map ->edges)
         (some #(and (magic? %) %))
         flatten
         (apply str)
         (#(Long/parseLong %)))))

(comment
  (time (euler-068 5))
  )