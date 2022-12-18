(ns caesarhu.clojure-euler.euler-088
  (:require [caesarhu.math.primes :as p]))

(defn generate-product-sum-map
  [limit]
  (let [product-sum-map (atom {})]
    (letfn [(product-sum
              [product sum num start]
              (let [k (+ product (- sum) num)] 
                (when (< k limit)
                  (swap! product-sum-map #(merge-with min %1 %2) {k product})
                  (doseq [i (range start (inc (* (quot limit product) 2)))]
                    (product-sum (* product i) (+ sum i) (inc num) i)))
                @product-sum-map))]
      (product-sum 1 1 1 2))))

(defn solve
  [limit]
  (->> (generate-product-sum-map (inc limit))
       (#(dissoc % 1))
       vals
       distinct
       (apply +)))

(comment
  (time (solve 12))
  )