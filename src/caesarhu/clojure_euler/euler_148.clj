(ns caesarhu.clojure-euler.euler-148
  (:require [clojure.math.numeric-tower :refer [expt]]
            [caesarhu.math.math-tools :refer [digits]]))

(defn euler-148
  [limit prime]
  (let [prime-prod (quot (* prime (inc prime)) 2)]
    (loop [[d & ds] (digits limit prime)
           sum 0
           current-prod 1]
      (if (nil? d)
        sum
        (recur ds
               (+ (* sum prime-prod) (quot (* current-prod d (inc d)) 2))
               (* (inc d) current-prod))))))

(comment
  (euler-148 (expt 10 9) 7)
  )