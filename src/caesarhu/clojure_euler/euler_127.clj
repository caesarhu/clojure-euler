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
    (reduce (fn [acc i]
              (conj acc [i (@rad i)]))
            (priority-map) (range 1 limit))))

(defn abc-hits
  [limit]
  (let [m (rad-map limit)
        valid-ab (fn [c x]
                   (let [y (- c x)]
                     (if (and (coprime? c x) (< (apply * (map m [x y c])) c))
                       [x y]
                       [])))
        valid-c (fn [c]
                  (when-let [qc (as-> (/ c (m c)) q (when (> q 2) q))]
                    (->> (subseq m < (sqrt qc))
                         (map first)
                         (filter #(< % c))
                         (mapcat #(valid-ab c %))
                         (apply sorted-set)
                         (#(subseq % < (/ c 2)))
                         (map #(vector % (- c %) c)))))]
    (mapcat valid-c (range 1 limit))))

(defn euler-127
  [limit]
  (->> (abc-hits limit)
       (map last)
       (apply +)))

(comment
  (time (euler-127 120000))
  )