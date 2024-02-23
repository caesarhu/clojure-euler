(ns caesarhu.clojure-euler.euler-197
  (:require [clojure.math.numeric-tower :refer [expt]]
            [caesarhu.math.cycle-detection :refer [floyd-detection-seq]]))

(def exp 30.403243784)
(def log2 (Math/log10 2))

(defn f
  [x]
  (if (zero? x) -1
      (*' (long (expt 2 (- exp (* x x))))
          (expt 10 -9))))

(def u-seq
  (iterate f 0))

(defn euler-197
  [n]
  (let [[head cycled] (floyd-detection-seq u-seq)
        length (count head)]
    (if (< n length)
      (+ (nth head n) (nth head (inc n)))
      (double (apply + cycled)))))

(comment
  (time (euler-197 (expt 10 12)))
  )
