(ns caesarhu.clojure-euler.euler-015
  (:require [caesarhu.math.math-tools :refer [binomial]]))

(defn euler-015
  [n]
  (binomial (* n 2) n))

(comment
  (time (euler-015 20))
  )