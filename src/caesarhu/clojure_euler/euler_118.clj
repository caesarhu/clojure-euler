(ns caesarhu.clojure-euler.euler-118
  (:require [caesarhu.math.primes :as p]
            [caesarhu.math.math-tools :refer [digits digits->number]]
            [clojure.math.combinatorics :as combo]))

(defn distinct-primes
  [limit]
  (let [v (->> (p/primes limit)
               (map (fn [p]
                      (let [d (digits p)
                            s (set d)]
                        (when (and (not (contains? s 0)) (= (count s) (count d)))
                          [p s]))))
               (filter some?))]
    {:prime-set (set (map first v))
     :digit-set (set (map last v))}))

(defn filter-partitions
  [digit-set]
  (->> (combo/partitions (range 1 10))
       (filter (fn [v]
                 (every? #(digit-set (set %)) v)))))

(defn count-primes
  [prime-set dv]
  (->> (combo/permutations dv)
       (map digits->number)
       (filter prime-set)
       count))

(defn euler-118
  []
  (let [m (distinct-primes 100000000)]
    (->> (filter-partitions (m :digit-set))
         (map #(map (partial count-primes (m :prime-set)) %))
         (map #(apply * %))
         (apply +))))

(comment
  (time (euler-118))
  )