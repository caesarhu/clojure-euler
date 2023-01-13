(ns caesarhu.clojure-euler.euler-127
  (:require [caesarhu.math.math-tools :refer [coprime?]] 
            [caesarhu.math.primes :as p]
            [clojure.math.numeric-tower :refer [sqrt]]))

(defn rad-range
  [limit]
  (let [rad (atom (vec (cons 0 (repeat (dec limit) 1))))]
    (doseq [p (p/primes)
            :while (< p limit)
            px (iterate #(+ p %) p)
            :while (< px limit)]
      (swap! rad update px * p))
    [@rad (reduce (fn [acc i] (update acc (@rad i) conj i))
                  (vec (repeat limit (sorted-set))) (range limit))]))

(defn abc-hits
  [limit]
  (let [[num-v rad-v] (rad-range limit)
        valid-ab (fn [c x]
                   (when-let [y (and (coprime? c x) (- c x))]
                     (when (< (apply * (map num-v [x y c])) c)
                       [x y])))
        mul-c (fn [c]
                (when-let [qc (as-> (/ c (num-v c)) q (when (> q 1) q))]
                  (->> (mapcat (fn [i] (subseq (rad-v i) < c)) (range 1 (sqrt qc)))
                       (mapcat (partial valid-ab c))
                       (apply sorted-set)
                       (#(subseq % < (/ c 2)))
                       (map #(vector % (- c %) c)))))]
    (mapcat mul-c (range 1 limit))))

(defn euler-127
  [limit]
  (->> (abc-hits limit)
       (map last)
       (apply +)))

(comment
  (time (euler-127 120000))
  )