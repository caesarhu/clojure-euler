(ns caesarhu.clojure-euler.euler-221
  (:require [caesarhu.math.primes :as p]
            [caesarhu.math.math-tools :refer [power-mod]]))

(defn gen-alexandrian
  [n]
  (let [p (inc (*' n n))
        d-seq (p/divisors p)]
    (for [d (take (quot (inc (count d-seq)) 2) d-seq)]
      (*' n (+' n d) (+' n (quot p d))))))

(defn euler-221
  [target]
  (loop [answer-set (sorted-set)
         p 1]
    (if (>= (count answer-set) (* 4 target))
      (nth (seq answer-set) (dec target))
      (recur (apply conj answer-set (gen-alexandrian p)) (inc p)))))

(comment
  (time (euler-221 150000))
  )
