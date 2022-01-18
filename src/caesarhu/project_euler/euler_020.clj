(ns caesarhu.project-euler.euler-020
  (:require [caesarhu.math.math-tools :as tools]))

(defn euler-020
  [n]
  (->> (tools/factorial n)
       tools/digits
       (apply +)))

(comment
  (time (euler-020 100))
  )