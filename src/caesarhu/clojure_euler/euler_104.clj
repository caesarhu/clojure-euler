(ns caesarhu.clojure-euler.euler-104
  (:require [clojure.math.numeric-tower :refer [sqrt expt round]]
            [caesarhu.math.math-tools :refer [digits digits->number]]))

(def golden-ratio (/ (+ 1 (sqrt 5)) 2))
(def log-golden-ratio (Math/log10 golden-ratio))
(def log-sqrt-5 (Math/log10 (sqrt 5)))

(defn fib-head
  [n]
  (let [log-raw (- (* n log-golden-ratio) log-sqrt-5)
        log-n (if (> log-raw 9)
                (+ log-raw (- (int log-raw)) 8)
                log-raw)
        result (expt 10 log-n)]
    (if (> log-n 8)
      (int result)
      (round result))))

(def fib-tail-seq
  (lazy-cat [0 1] (map #(mod (+ %1 %2) (expt 10 9)) (rest fib-tail-seq) fib-tail-seq)))

(def fib-head-seq
  (map fib-head (range)))

(def fib-seq
  (map vector fib-head-seq fib-tail-seq))

(defn pandigital?
  [x]
  (let [s (take 9 (digits x))]
    (= [1 2 3 4 5 6 7 8 9] (sort s))))

(defn euler-104
  []
  (->> (map vector (range) fib-seq)
       (filter #(every? pandigital? (last %)))
       first))

(comment
  (take 50 fib-seq)
  (time (euler-104))
  )