(ns caesarhu.clojure-euler.euler-225)

(def tribonacci
  (memoize
   (fn [n]
     (if (<= n 3) 1
         (+ (tribonacci (- n 1))
            (tribonacci (- n 2))
            (tribonacci (- n 3)))))))

(def tribonacci-seq
  (map tribonacci (iterate inc 1)))

(comment
  tribonacci-seq
  )
