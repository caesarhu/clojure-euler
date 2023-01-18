(ns caesarhu.clojure-euler.euler-135
  (:require [clojure.math.numeric-tower :refer [ceil sqrt]]))

(defn differences
  [limit]
  (let [result (atom (vec (repeat limit 0)))]
    (doseq [u (range 1 limit)
            v (iterate inc (quot u 3))
            :let [n (* u v)
                  x (/ (- (* 3 v) u) 4)]
            :while (< n limit)
            :when (pos-int? x)]
      (swap! result update n inc))
    @result))

(defn solve
  [limit]
  (->> (differences limit)
       (filter #(= 10 %))
       count))

(defn square
  [n]
  (* n n))

(defn differences-seq
  [limit]
  (for [d (range 1 (/ (inc limit) 4))
        :let [d2 (* 2 d)]
        u (range (dec d2) -1 -1)
        :let [n (- (square d2) (square u))]
        :while (< n limit)
        x (set [(+ d2 u) (- d2 u)])
        :when (> x d)]
    [n d [(+ x d) x (- x d)]]))

(defn euler-135
  [limit target]
  (let [result (atom (vec (repeat limit 0)))]
    (doseq [d (range 1 (/ (inc limit) 4))
            :let [d2 (* 2 d)]
            k (range (dec d2) -1 -1)
            :let [n (- (square d2) (square k))]
            :while (< n limit)]
      (if (< 0 k d)
        (swap! result update n #(+ 2 %))
        (swap! result update n inc)))
    (count (filter #(= target %) @result))))

(comment
  (def tt (differences 1000000))
  (->> (map vector (range) tt)
       (filter #(= 1 (last %))))
  (time (euler-135 1000000 10))
  )