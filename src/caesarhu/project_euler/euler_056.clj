(ns caesarhu.project-euler.euler-056
  (:require [clojure.math.numeric-tower :as math]
            [caesarhu.math.math-tools :refer [digits]]))

(defn euler-056
  [limit]
  (->> (for [i (range 1 100)
             j (range 1 100)]
         (->> (math/expt i j) digits (apply +)))
       (apply max)))

(comment
  (time (euler-056 100))
  )