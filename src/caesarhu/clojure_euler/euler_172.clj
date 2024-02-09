(ns caesarhu.clojure-euler.euler-172
  (:require [clojure.math.combinatorics :as c]))

(defn combinations
  [sum max-value max-items current]
  (cond
    (zero? sum) [(concat current (repeat max-items 0))]
    (and (pos? sum) (>= (* max-items max-value) sum))
    (->> (for [items (range (inc max-items) -1 -1)]
           (combinations (- sum (* max-value items))
                         (dec max-value)
                         (- max-items items)
                         (concat current (repeat items max-value))))
         (apply concat))))

(defn euler-172
  [n max-value]
  (->> (combinations 18 3 10 [])
       (map (fn [ds]
              (*' (c/count-permutations ds)
                  (c/count-permutations (mapcat #(repeat %1 %2) ds (range 10))))))
       (apply +')
       (*' 9/10)
       long))

(comment
  (combinations 18 3 10 [])
  (time (euler-172 18 3))
  )
