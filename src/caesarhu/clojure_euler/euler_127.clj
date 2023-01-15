(ns caesarhu.clojure-euler.euler-127
  (:require [caesarhu.math.math-tools :refer [coprime?]]
            [clojure.data.priority-map :refer [priority-map]]
            [caesarhu.math.primes :as p]
            [clojure.math.numeric-tower :refer [sqrt]]))

(defn rad-map
  [limit]
  (let [rad (atom (vec (repeat limit 1)))]
    (doseq [p (p/primes)
            :while (< p limit)
            px (iterate #(+ p %) p)
            :while (< px limit)]
      (swap! rad update px * p))
    (->> (range 1 limit)
         (map #(vector % (@rad %)))
         (into (priority-map)))))

(defn abc-hits
  [limit]
  (let [m (rad-map limit)
        valid-ab (fn [c x]
                   (let [y (- c x)
                         upper-bound (/ c (* (m x) (m c)))]
                     (when (and (< x c) (coprime? (m c) (m x)) (< 1 (m y) upper-bound))
                       (sorted-set x y c))))
        valid-c (fn [c]
                  (when-let [qc (as-> (/ c (m c)) q (when (> q 2) q))]
                    (->> (subseq m < (sqrt qc))
                         (map first)
                         (map #(valid-ab c %))
                         (remove nil?)
                         set)))]
    (mapcat valid-c (range 1 limit))))

(defn euler-127
  [limit]
  (->> (abc-hits limit)
       (map last)
       (apply +)))

(comment
  (subseq (->> (map vector (range 10) (range 10))
               (into (sorted-map))) < 5)
  (time (->> (euler-127 120000)))
  )