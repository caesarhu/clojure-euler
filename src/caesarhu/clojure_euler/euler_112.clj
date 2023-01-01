(ns caesarhu.clojure-euler.euler-112
  (:require [caesarhu.math.math-tools :refer [digits]]))

(defn bouncy-number?
  [n]
  (when-let [ds (and (> n 100) (digits n))]
    (not (or (apply >= ds) (apply <= ds)))))

(defn euler-112
  [percent]
  (let [target (/ percent 100)]
    (loop [i 101
           bouncy-numbers 0]
      (let [new-bouncy-numbers (if (bouncy-number? i)
                                 (inc bouncy-numbers)
                                 bouncy-numbers)]
        (if (>= (/ new-bouncy-numbers i) target)
          i
          (recur (inc i) new-bouncy-numbers))))))

(comment
  (time (euler-112 99))
  )