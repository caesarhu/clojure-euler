(ns caesarhu.clojure-euler.euler-207
  (:require [caesarhu.math.polynomial :refer [quadratic-root]]
            [clojure.math.numeric-tower :refer [expt ceil]]))

(defn k->n
  [k]
  (->> (quadratic-root 1 1 (- k)) (some #(and (pos? %) %)) long))

(defn ft
  [t]
  (let [x (expt 2 t)
        k (* x (dec x))
        n (k->n k)]
    [t k n]))

(defn P
  [n]
  (let [x (k->n n)
        t (->> (iterate inc 1) (map ft) (take-while #(<= (second %) n)) last first)]
    (/ t x)))

(defn n->k
  [n]
  (* n (inc n)))

(defn calc-n
  [r t]
  (let [n (/ t r)]
    (if (= n (ceil n))
      (inc n)
      (ceil n))))

(defn euler-207
  [r]
  (loop [t 1]
    (let [[_ _ next-n] (ft (inc t))
          n (calc-n r t)]
      (if (> next-n n)
        (long (n->k n))
        (recur (inc t))))))

(comment
  (time (euler-207 1/12345))
  )
