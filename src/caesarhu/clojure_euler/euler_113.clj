(ns caesarhu.clojure-euler.euler-113
  (:require [clojure.math.combinatorics :as combo]))

(def init-vector (map vec (combo/cartesian-product (range 10) (range 10))))
(def init-map
  (->> init-vector
       (map (fn [[start end :as v]]
              [v (sorted-map 1 (if (or (zero? start) (not= start end)) 0 1))]))
       (into (sorted-map))))

(defn next-key-map
  [m k]
  (let [[start end] k
        step (if (pos? (- start end)) -1 1)
        next-k (-> (m k) last first)
        sum (apply + (for [i (range start (+ end step) step)]
                       (get-in m [[start i] next-k])))]
    (merge-with merge m {k {(inc next-k) sum}})))

(defn next-map
  [m]
  (reduce (fn [acc k]
            (next-key-map acc k))
          m init-vector))

(defn euler-113
  [expt]
  (let [m (nth (iterate next-map init-map) (dec expt))]
    (->> m 
         vals
         (mapcat vals)
         (apply +))))

(comment
  (time (euler-113 100))
  )