(ns caesarhu.clojure-euler.euler-124
  (:require [caesarhu.math.primes :as p]
            [clojure.math.numeric-tower :refer [expt]]))

(defn power-of-prime
  [limit p]
  (->> (for [px (map #(expt p %) (iterate inc 1))
             :while (<= px limit)]
         (for [t (iterate inc 1)
               :let [tpx (* t px)]
               :while (<= tpx limit)]
           {tpx {p 1}}))
       flatten
       (apply merge-with (fn [m1 m2] (merge-with + m1 m2)))))

(defn factors-range
  [limit]
  (->> (for [p (take-while #(< % limit) (p/primes))]
         (power-of-prime limit p))
       (apply merge-with (fn [m1 m2] (merge-with + m1 m2)) (sorted-map))))

(defn rad
  [m]
  (apply * (keys m)))

(defn euler-124
  [limit target]
  (nth (->> (factors-range limit)
            (map #(vector (key %) (rad (val %))))
            (sort-by last)) (- target 2)))

(comment
  (factors-range 40)
  (time (euler-124 100000 10000))
  )