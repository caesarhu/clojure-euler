(ns caesarhu.clojure-euler.euler-206
  (:require [caesarhu.math.math-tools :refer [digits->number digits]]
            [clojure.math.numeric-tower :refer [sqrt expt ceil]]
            [clojure.math.combinatorics :as c]))


(defn square
  [n]
  (* n n))

(def target-seq [1 :x 2 :x 3 :x 4 :x 5 :x 6 :x 7 :x 8 :x 9 :x 0])

(defn target-seq?
  [s]
  (let [length (count s)]
    (->> (map #(vector %1 %2) (take length target-seq) s)
         (remove #(= :x (first %)))
         (every? #(apply = %)))))

(defn target-n?
  [n]
  (when-not (zero? n)
    (->> (square n) digits target-seq?)))

(defn euler-206
  []
  (->> (range 138902663 101010101 -1)
       (map #(* 10 %))
       (filter target-n?)
       first))

(comment
  (time (euler-206))
  )
