(ns caesarhu.clojure-euler.euler-137
  (:require [clojure.math.numeric-tower :refer [expt]]))

(def fib-seq
  ((fn fib [a b]
     (lazy-seq
      (cons b (fib b (+ a b)))))
   1 1))

(def golden-nugget
  (->> (partition 2 fib-seq)
       (map #(apply * %))))

(defn euler-137
  [target]
  (nth golden-nugget (dec target)))


(comment
  (euler-137 13)
  )