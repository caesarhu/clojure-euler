(ns caesarhu.clojure-euler.euler-138
  (:require [caesarhu.math.pell-equation :refer [pell-solutions]]))

(defn pell->bL
  [[x y]]
  (let [mx (mod x 5)]
    [(* 2 (if (= 3 mx)
            (/ (+ x 2) 5)
            (/ (- x 2) 5)))
     y]))

(defn euler-138
  [limit]
  (->> (pell-solutions 5 -1)
       rest
       (map last)
       (take limit)
       (apply +)))

(comment
  (->> (pell-solutions 5 -1)
       rest
       (map pell->bL))
  (euler-138 12)
  )