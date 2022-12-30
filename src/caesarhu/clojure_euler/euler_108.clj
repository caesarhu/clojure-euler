(ns caesarhu.clojure-euler.euler-108
  (:require [caesarhu.math.primes :as p]
            [clojure.math.numeric-tower :as math]
            [clojure.data.priority-map :refer [priority-map]]))

(defn square-divisors
  [n]
  (->> (p/factors n)
       frequencies
       vals
       (map #(inc (* 2 %)))
       (apply *)
       (#(math/ceil (/ % 2)))))

(defn brute-force
  [limit]
  (->> (iterate inc 2)
       (map #(vector (square-divisors %) %))
       (filter #(> (first %) limit))
       first))

;-------------------------------------------------------------

(defn v->number
  [s]
  (->> (map #(math/expt %1 %2) (p/primes) s)
       (apply *')))

(defn v->divisors
  [v]
  (->> v (map #(inc (* 2 %))) (apply *) (#(math/ceil (/ % 2))) int))

(defn extend-vector
  [v]
  (let [v0 (vec (cons (inc (first v)) (rest v)))
        v2 (vec (partition 2 1 v))]
    (concat [v0]
            (for [i (range (count v2))
                  :let [[n1 n2] (v2 i)]
                  :when (> n1 n2)]
              (assoc v (inc i) (inc n2)))
            [(assoc v (count v) 1)])))

(defn euler-108
  [limit]
  (let [limit-over-log5 (-> (/ (Math/log10 limit) (Math/log10 5)) math/ceil int)
        init-vector (vec (repeat limit-over-log5 1))
        filter-limit (fn [m]
                       (some->> m reverse
                                (take-while #(> (last %) limit))
                                not-empty
                                (map first)))]
    (loop [m (priority-map init-vector (v->divisors init-vector))]
      (let [vs (filter-limit m)]
        (if (some #(= limit-over-log5 (count %)) vs)
          (->> (map #(vector (v->number %) %) vs)
               (apply min-key first))
          (recur (->> (mapcat extend-vector (keys m))
                      (map #(hash-map % (v->divisors %)))
                      (apply merge m))))))))

(comment
  (time (euler-108 1000))
  )