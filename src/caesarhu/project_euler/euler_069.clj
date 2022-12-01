(ns caesarhu.project-euler.euler-069
  (:require [caesarhu.math.primes :as p]))

(defn totient-ratio
  [n]
  (/ n (p/totient n)))

(defn euler-069-slow
  [limit]
  (->> (range 2 (inc limit))
       (apply max-key totient-ratio)))

(defn euler-069
  [limit]
  (loop [primes (p/primes)
         result 1]
    (let [next-result (* result (first primes))]
      (if (> next-result limit)
        result
        (recur (rest primes) next-result)))))

(comment
  (totient-ratio 840)
  (time (euler-069 1000000))
  )