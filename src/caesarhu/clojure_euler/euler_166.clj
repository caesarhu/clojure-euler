(ns caesarhu.clojure-euler.euler-166
  (:require [clojure.set :refer [union intersection]]))

(defn euler-166
  []
  (let [dset (set (range 10))
        m2 (apply merge-with union (for [i (range 10)
                                         j (range 10)]
                                     {(+ i j) #{[i j]}}))
        sum-map (->> (for [[sum1 s1] m2
                           [sum2 s2] m2
                           [d1 d4] s1
                           [d2 d3] s2
                           :let [sum (+ sum1 sum2)]
                           :when (<= sum 18)
                           [d5 d8] (m2 sum2)
                           [d6 d7] (m2 sum1)]
                       (let [valid-x? (fn [x] (every? dset [(- sum x d1 d5) (- sum x d2 d7) (- (+ x d1 d5) d6 d3)]))
                             valid-y? (fn [y] (every? dset [(- sum y d1 d8) (- sum y d2 d6) (- (+ y d1 d8) d7 d3)]))
                             count-xy (* (count (filter valid-x? (range 10)))
                                         (count (filter valid-y? (range 10))))]
                         {sum count-xy}))
                     (apply merge-with +))]
    (->> (range 18)
         (map #(sum-map %))
         (apply +)
         (* 2)
         (+ (sum-map 18)))))

(comment
  (time (euler-166))
  )
