(ns caesarhu.clojure-euler.euler-145
  (:require [clojure.math.numeric-tower :refer [expt]]))

(defn reversible-digits
  [d head?]
  (let [s (if head? (range 1 10) (range 10))
        valid? (fn [n] (and (odd? (+ d n))
                            (< (+ d n) 10)))]
    (filter valid? s)))

(defn count-reversible
  []
  [(->> (range 1 10)
        (mapcat #(reversible-digits % true))
        count)
   (->> (range 10)
        (mapcat #(reversible-digits % false))
        count)])

(defn euler-145
  [power]
  (->> (for [i (range (inc power))]
         (cond
           (< i 2) 0
           (even? i) (* 20 (expt 30 (quot (- i 2) 2)))
           (= 3 (mod i 4)) (* 20 5 (expt (* 20 25) (quot (- i 3) 4)))
           :else 0))
       (apply +)))

(comment
  (count-reversible)
  (time (euler-145 9))
  )