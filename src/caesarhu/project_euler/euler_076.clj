(ns caesarhu.project-euler.euler-076
  (:require [clojure.core.memoize :as m]
            [clojure.math.numeric-tower :as math]))

(defn next-sum
  [[a b :as s]]
  (if (and b (< a b))
    [(cons (inc a) (rest s))
     (cons 1 s)]
    [(cons 1 s)]))

(defn sum-seq
  [n]
  (->> (reduce (fn [state i]
                 (cons [i]
                       (mapcat next-sum state)))
               [[1]] (range 2 (inc n)))
       rest))

(defn brute-force
  [n]
  (count (sum-seq n)))

;=========================================================================
;https://en.wikipedia.org/wiki/Partition_(number_theory)
;=========================================================================

(defn count-sum
  [limit]
  (let [sum (atom (vec (repeat (inc limit) 1)))]
    (doseq [i (range 2 (inc limit))
            j (range i (inc limit))]
      (let [sj (@sum j)
            sj-i (@sum (- j i))]
        (swap! sum assoc j (+ sj sj-i))))
    @sum))

(defn count-sum2
  [limit]
  (loop [sum (into {} (for [i (range (inc limit))]
                        [i 1]))
         i 2]
    (if (> i limit) sum
        (let [new-sum (loop [jsum sum
                             j i]
                        (if (> j limit) jsum
                            (recur (merge-with + jsum {j (jsum (- j i))}) (inc j))))]
          (recur new-sum (inc i))))))

(defn euler-076
  [n]
  (-> (count-sum2 n) (#(% n)) dec))

(comment
  (time (euler-076 100))
  )