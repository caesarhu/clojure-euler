(ns caesarhu.project-euler.euler-029
  (:require [clojure.math.numeric-tower :refer [expt]]))

(defn euler-029
  [limit]
  (->> (for [i (range 2 (inc limit))
             j (range 2 (inc limit))]
         (expt i j))
       distinct
       count))

(comment
  (time (euler-029 100))
  )