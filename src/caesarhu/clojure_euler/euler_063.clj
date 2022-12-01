(ns caesarhu.clojure-euler.euler-063
  (:require [clojure.math.numeric-tower :as math]))

(defn power-seq
  [power]
  (->> (map #(math/expt % power) (iterate inc 1))
       (drop-while #(< % (math/expt 10 (dec power))))
       (take-while #(< % (math/expt 10 power)))))

(defn euler-063
  []
  (->> (map power-seq (iterate inc 1))
       (take-while not-empty)
       flatten
       count))

(comment
  (time (euler-063))
  )