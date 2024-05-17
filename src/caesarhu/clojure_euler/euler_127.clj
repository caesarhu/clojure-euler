(ns caesarhu.clojure-euler.euler-127
  (:require [caesarhu.math.math-tools :refer [coprime?]]
            [clojure.data.priority-map :refer [priority-map]]
            [caesarhu.math.primes :as p :refer [range-factors]]
            [clojure.math.numeric-tower :refer [sqrt]]
            [clojure.set :refer [union]]))

(defn rad-map
  [^long limit]
  (->> (range-factors limit)
       (map #(apply * (keys %)))
       (map vector (range))
       (drop 1)
       (into (priority-map))))

(defn abc-hits
  [^long limit]
  (let [m (rad-map limit)
        valid-ab (fn [c a]
                   (let [b (- c a)
                         upper-bound (/ c (* (m a) (m c)))]
                     (when (and (< a c) (coprime? (m c) (m a)) (< 1 (m b) upper-bound))
                       (vec (sort [a b c])))))
        valid-c (fn [c]
                  (when-let [qc (let [q (/ c (m c))]
                                  (when (> q 2) q))]
                    (->> (subseq m < (sqrt qc))
                         (map first)
                         (map #(valid-ab c %))
                         (remove nil?)
                         set)))]
    (mapcat valid-c (range 1 limit))))

(defn euler-127
  [^long limit]
  (->> (abc-hits limit)
       (map last)
       (apply +)))

(comment
  (time (euler-127 120000))
  )
