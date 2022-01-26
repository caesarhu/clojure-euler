(ns caesarhu.project-euler.euler-066
  (:require [caesarhu.math.pell-equation :as pell]
            [caesarhu.math.math-tools :refer [square?]]))

(defn euler-066
  [limit]
  (->> (range 2 (inc limit))
       (remove square?)
       (map #(vector % (ffirst (pell/pell-solutions %))))
       (apply max-key last)
       first))

(comment
  (time (euler-066 1000))
  )