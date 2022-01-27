(ns caesarhu.project-euler.euler-072
  (:require [caesarhu.math.primes :as p]))

(defn tt
  [limit]
  (->> (range 2 (inc limit))
       (map p/totient)
       (apply +)))

(comment
  (time (tt 1000000))
  )