(ns caesarhu.clojure-euler.euler-016
  (:require [clojure.math.numeric-tower :refer [expt]]
            [caesarhu.math.math-tools :refer [digits]]))

(defn euler-016
  [e]
  (->> (expt 2 e)
       digits
       (apply +)))

(comment
  (time (euler-016 1000))
  )