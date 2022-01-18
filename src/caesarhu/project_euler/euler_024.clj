(ns caesarhu.project-euler.euler-024
  (:require [clojure.math.combinatorics :as combo]
            [caesarhu.math.math-tools :refer [digits->number]]))

(defn solve
  [n]
  (->> (combo/nth-permutation (range 10) (dec n))
       digits->number))

(comment
  (time (solve 1000000))
  )