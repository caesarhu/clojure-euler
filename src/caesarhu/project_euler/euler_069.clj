(ns caesarhu.project-euler.euler-069
  (:require [caesarhu.math.primes :as p]))

(defn euler-069
  [limit]
  (let [totient-ratio (fn [n] (/ n (p/totient n)))]
    (->> (range 2 (inc limit))
         (apply max-key totient-ratio))))

(comment
  (time (euler-069 1000000))
  )