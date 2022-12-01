(ns caesarhu.clojure-euler.euler-005
  (:require [caesarhu.math.math-tools :refer [lcm*]]))

(defn euler-005
  [n]
  (apply lcm* (range 1 (inc n))))

(comment
  (time (euler-005 20))
  )