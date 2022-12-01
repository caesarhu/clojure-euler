(ns caesarhu.clojure-euler.euler-040
  (:require [clojure.math.numeric-tower :as math]
            [caesarhu.math.math-tools :as tools]))

(def digit-seq
  (mapcat tools/digits (rest (range))))

(defn answer-seq
  [n]
  (->> (for [i (range n)]
         (math/expt 10 i))
       (map #(nth digit-seq (dec %)))))

(defn euler-040
  [n]
  (apply * (answer-seq n)))

(comment
  (time (euler-040 7))
  )