(ns caesarhu.clojure-euler.euler-095
  (:require [caesarhu.math.cycle-detection :refer [floyd-cycle? floyd-detection]]))

(defn generate-sum-map
  [limit]
  (let [m (apply merge (for [i (range 2 (inc limit))]
                         {i 1}))]
    (->> (for [i (iterate inc 2)
               :while (<= (* i i) limit)]
           (for [j (iterate inc i)
                 :while (<= (* i j) limit)]
             {(* i j) (+ i j)}))
         (apply concat)
         (apply merge-with + m))))

(defn euler-095
  [limit]
  (let [sum-map (atom (generate-sum-map limit))
        take-n-map (fn [start n]
                     (if (= start n)
                       (cons start (take-while #(and % (not= n %)) (iterate @sum-map (@sum-map start))))
                       (take-while #(and % (not= n %)) (iterate @sum-map start))))
        chain (fn [n]
                (let [[x length] (floyd-detection @sum-map n)
                      s (take-n-map n x)]
                  (apply swap! sum-map dissoc s)
                  (cond
                    (= n x) [(apply min s) length]
                    (int? x) (recur x))))]
    (->> (range limit 5 -1)
         (map chain)
         (filter some?)
         (apply max-key last))))

(comment
  (time (generate-sum-map 1000000))
  (time (euler-095 1000000))
  )