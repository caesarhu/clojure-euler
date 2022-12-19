(ns caesarhu.clojure-euler.euler-088
  (:require [caesarhu.math.primes :as p]))

(defn product-loop
  [limit result-map]
  (letfn [(product-sum [product sum c start] ; p:product s:sum c:count of factors
            (let [k (+ product (- sum) c)]
              (when (<= k limit)
                (swap! result-map #(merge-with min %1 %2) {k product})
                (doseq [i (iterate inc start)
                        :let [next-product (* product i)]
                        :while (<= next-product (* 2 limit))]
                  (product-sum next-product (+ sum i) (inc c) i)))))]
    (product-sum 1 1 1 2)))

(defn euler-088-atom
  [limit]
  (let [result-map (atom {})]
    (product-loop limit result-map)
    (->> (dissoc @result-map 1)
         vals
         set
         (apply +))))

(comment
  (time (euler-088-atom 12000))
  )