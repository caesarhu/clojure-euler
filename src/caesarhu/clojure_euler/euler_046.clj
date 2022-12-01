(ns caesarhu.clojure-euler.euler-046
  (:require [caesarhu.math.primes :as p]
            [clojure.math.numeric-tower :as math]))

(defn square*2
  [n]
  (* 2 n n))

(defn conjecture?
  [n]
  (some true? (for [i (range 1 (math/sqrt n))]
                (p/is-prime? (- n (square*2 i))))))

(defn euler-046
  []
  (->> (iterate #(+ % 2) 9)
       (remove p/is-prime?)
       (some #(and ((complement conjecture?) %) %))))

(comment
  (time (euler-046))
  )