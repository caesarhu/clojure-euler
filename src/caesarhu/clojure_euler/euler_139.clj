(ns caesarhu.clojure-euler.euler-139
  (:require [caesarhu.math.pell-equation :refer [pell-solutions]]
            [clojure.math.numeric-tower :refer [expt]]))

(defn euler-139
  [limit]
  (->> (pell-solutions 2 -1)
       rest
       (map #(apply + %))
       (take-while #(<= % limit))
       (map #(quot limit %))
       (apply +)))

(comment
  (time (euler-139 (expt 10 8)))
  )
