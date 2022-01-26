(ns caesarhu.project-euler.euler-065
  (:require [caesarhu.math.math-tools :as tools]))

(def e-repeating-seq
  (lazy-seq (cons 2 (mapcat #(vector %1 (* 2 %2) %1) (repeat 1) (iterate inc 1)))))

(defn infinite-continued-fraction [s iterations]
  (let [s (reverse (take iterations s))]
    (loop [xs (rest s)
           result (first s)]
      (if (seq xs)
        (recur (rest xs) (+ (first xs) (/ 1 result)))
        result))))

(defn infinite-continued-fraction-seq [s]
  (let [f (partial infinite-continued-fraction s)]
    (map f (iterate inc 1))))

(defn e [iterations]
  (nth
   (infinite-continued-fraction-seq e-repeating-seq)
   (dec iterations)))

(defn euler-065
  [iterations]
  (->> (numerator (e iterations))
       (tools/digits)
       (apply +)))

(comment
  (time (euler-065 100))
  )