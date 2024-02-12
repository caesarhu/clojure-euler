(ns caesarhu.clojure-euler.euler-181
  (:require [clojure.math.combinatorics :as c]))

(defn brute-force
  [s]
  (count (c/partitions s)))

(comment
  (->> (c/partitions (repeat 5 0)))
  )
