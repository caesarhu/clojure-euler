(ns caesarhu.clojure-euler.euler-156
  (:require [caesarhu.math.math-tools :refer [digits digits->number]]
            [clojure.math.numeric-tower :refer [expt]]))

(defn f
  [n d]
  (loop [ds (digits n)
         result 0]
    (if (empty? ds)
      result
      (let [id (first ds)
            ie (dec (count ds))
            ir (* id ie (expt 10 (dec ie)))
            current (cond
                      (> id d) (+ ir (expt 10 ie))
                      (= id d) (+ ir 1 (digits->number (rest ds)))
                      :else ir)]
        (recur (rest ds) (+ result current))))))

(defn range?
  [left right d]
  (and (>= (f right d) left)
       (<= (f left d) right)))

(defn search-numbers
  [left right d]
  (if (< (- right left) 10)
    (->> (for [i (range left (inc right))
               :when (= (f i d) i)]
           i)
         (apply +))
    (let [middle (quot (+ left right) 2)]
      (+ (if (range? left middle d)
           (search-numbers left middle d)
           0)
         (if (range? (inc middle) right d)
           (search-numbers (inc middle) right d)
           0)))))

(defn euler-156
  []
  (let [limit (expt 10 11)]
    (->> (for [i (range 1 10)]
           (search-numbers 1 limit i))
         (apply +))))

(comment
  (search-numbers 1 (expt 10 11) 1)
  (time (euler-156))
  )
