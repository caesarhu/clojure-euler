(ns caesarhu.clojure-euler.euler-025
  (:require [caesarhu.math.math-tools :refer [digits]]
            [clojure.math.numeric-tower :refer [expt]]))

(def fib-seq
  ((fn fib [a b]
     (lazy-seq (cons a (fib b (+' a b)))))
   0 1))

(defn euler-025
  []
  (->> (map vector fib-seq (range))
       (some #(and (>= (first %) (expt 10 999)) %))
       last))

(comment
  (time (euler-025))
  )