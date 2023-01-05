(ns caesarhu.clojure-euler.euler-114)

; https://github.com/lvlte/project-euler/blob/main/problems/101-150/114-Counting_block_combinations_I.js

(defn next-combinations
  [s]
  (let [[a-4 a-3 a-2 a-1] (take-last 4 s)
        result (+' (*' 2 a-1) (-' a-2) a-4)]
    [a-3 a-2 a-1 result]))

(def combinations-seq
  (rest (map first (iterate next-combinations [1 1 1 1]))))

(defn euler-114
  [n]
  (nth combinations-seq n))

(comment
  (take 10 combinations-seq)
  (time (euler-114 30))
  )
