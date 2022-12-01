(ns caesarhu.clojure-euler.euler-027
  (:require [caesarhu.math.primes :as p]))

(defn quadratic
  [a b]
  (fn [n]
    (+ (* n n) (* a n) b)))

(defn euler-027
  []
  (->> (for [a (range -999 1000)
             b (range -1000 1001)]
         (let [qn (quadratic a b)
               length (->> (take-while #(p/is-prime? (qn %)) (range))
                           count)]
           [length (* a b) [a b]]))
       (apply max-key first)))

(comment
  (time (euler-027))
  )