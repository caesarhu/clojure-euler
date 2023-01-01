(ns caesarhu.clojure-euler.euler-110
  (:require [caesarhu.math.primes :as p]
            [clojure.math.numeric-tower :refer [expt ceil]]
            [clojure.data.priority-map :refer [priority-map]]))

(defn v->number
  [v]
  (->> (map #(expt %1 %2) (p/primes) v)
       (apply *')))

(defn v->divisors
  [v]
  (->> v (map #(inc (* 2 %))) (apply *') (#(ceil (/ % 2)))))

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

(defn euler-110
  [limit]
  (let [limit-over-log5 (-> (/ (Math/log10 (* 2 limit)) (Math/log10 5)) ceil int)
        init-vector (vec (repeat limit-over-log5 1))
        filter-limit (fn [m]
                       (some->> m reverse
                                (take-while #(> (last %) limit))
                                not-empty
                                (map first)))]
    (loop [m (priority-map init-vector (v->divisors init-vector))]
      (let [vs (filter-limit m)]
        (if (some #(= limit-over-log5 (count %)) vs)
          [(v->number (last vs)) (last vs)]
          (recur (->> (mapcat extend-vector (keys m))
                      (map #(hash-map % (v->divisors %)))
                      (apply merge m))))))))

(comment
  (time (euler-110 4000000))
  )