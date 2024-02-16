(ns caesarhu.clojure-euler.euler-184
  (:require [clojure.set :refer [union]]))

(defn square
  [n]
  (* n n))

(defn points
  [r]
  (->> (for [x (range r)
             y (range r)
             :when (< (+ (square x) (square y)) (square r))]
         (set (for [xx [x (- x)]
                    yy [y (- y)]]
                [xx yy])))
       (apply union)))

(defn slope-data
  [r]
  (let [r2 (square r)
        n (atom 0)
        m (->> (for [x (range r)
                     y (range 1 r)
                     :when (< (+ (square x) (square y)) r2)]
                 (do
                   (swap! n inc)
                   (sorted-map (/ x y) 1)))
               (apply merge-with +))]
    [@n m]))

(defn euler-184
  [r]
  (let [[n slope-map] (slope-data r)]
    (loop [num-above 0
           num-below n
           degens (vals slope-map)
           result 0]
      (if (not-empty degens)
        (let [degen (first degens)
              new-num-below (- num-below degen)
              np4 (* 4 degen (+ n num-above) new-num-below)]
          (recur (+ num-above degen)
                 new-num-below
                 (rest degens)
                 (+ result np4)))
        result))))

(comment
  (time (euler-184 1200))
  )
