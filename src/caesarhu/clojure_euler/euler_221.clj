(ns caesarhu.clojure-euler.euler-221
  (:require [clojure.math.numeric-tower :refer [sqrt]]))

(defn gen-alexandrian
  [^long n]
  (if (= n 1) [6]
      (let [p (inc (*' n n))]
        (for [d (range 1 n)
              :when (zero? (mod p d))]
          (*' n (+' n d) (+' n (quot p d)))))))

(defn euler-221
  [target]
  (loop [answer-set (sorted-set)
         n 1]
    (if (>= (count answer-set) (long (* 3.5 target)))
      (nth (seq answer-set) (dec target))
      (recur (apply conj answer-set (gen-alexandrian n)) (inc n)))))

(comment
  (time (euler-221 150000))
  )
