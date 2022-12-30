(ns caesarhu.clojure-euler.euler-110
  (:require [caesarhu.math.primes :as p]
            [clojure.math.numeric-tower :as math]
            [clojure.data.priority-map :refer [priority-map]]))

(defn v->number
  [s]
  (->> (map #(math/expt %1 %2) (p/primes) s)
       (apply *')))

(defn v->divisors
  [v]
  (->> v (map #(inc (* 2 %))) (apply *) (#(math/ceil (/ % 2))) int))

(defn append-vector
  [v x]
  (let [length (count v)]
    (assoc v length x)))

(defn inc-vector
  [v]
  (let [v0 (vec (cons (inc (first v)) (rest v)))
        v2 (vec (partition 2 1 v))]
    (->> (for [i (range (count v2))
               :let [[n1 n2] (v2 i)]
               :when (> n1 n2)]
           (assoc v (inc i) (inc n2)))
         (cons v0))))

(defn extend-vector
  [v]
  (concat (inc-vector v) [(append-vector v 1)]))

(defn accept-result?
  [m limit]
  (let [[v1 d1] (last m)
        [v2 d2] (->> m reverse (take-while #(> (last %) limit)) last)]
    (and (> d1 limit) (>= (/ (count v1) (count v2)) 3/2))))

(defn euler-110
  [limit]
  (loop [m (priority-map [1] 2)]
    (if (accept-result? m limit)
      (->> m
           reverse
           (take-while #(> (last %) limit))
           (map first)
           (map v->number)
           (apply min))
      (recur (->> (mapcat extend-vector (keys m)) 
                  (map #(hash-map % (v->divisors %)))
                  (apply merge m))))))

(comment
  (time (euler-110 4000000))
  )