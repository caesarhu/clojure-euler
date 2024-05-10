(ns caesarhu.clojure-euler.euler-002
  (:require [clojure.math.numeric-tower :refer [sqrt round expt]]))

(def fib-seq
  ((fn fib [a b]
     (lazy-seq (cons a (fib b (+ a b)))))
   0 1))

(defn euler-002-old
  [limit]
  (->> (take-while #(< % limit) fib-seq)
       (filter even?)
       (apply +)))

(comment
  (time (euler-002-old 4000000))
  )

(def golden-ratio (/ (inc (sqrt 5)) 2))
(def R (expt golden-ratio 3))

(def f-seq
  (iterate #(round (* R %)) 2))

(defn euler-002
  [^long limit]
  (->> (take-while #(<= % limit) f-seq)
       (apply +)))

(comment
  (time (euler-002 4000000))
  )
