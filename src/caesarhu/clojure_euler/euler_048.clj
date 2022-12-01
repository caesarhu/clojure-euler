(ns caesarhu.clojure-euler.euler-048
  (:require [caesarhu.math.math-tools :as tools]
            [clojure.math.numeric-tower :as math]))

(defn euler-048
  [limit]
  (->> (range 1 (inc limit))
       (map #(tools/power-mod % % (math/expt 10 10)))
       (apply +)
       (#(mod % (math/expt 10 10)))))

(comment
  (time (euler-048 1000))
  )