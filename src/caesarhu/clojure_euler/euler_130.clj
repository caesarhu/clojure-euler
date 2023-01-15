(ns caesarhu.clojure-euler.euler-130
  (:require [caesarhu.math.primes :as p]
            [caesarhu.math.math-tools :refer [coprime?]]))

(defn repunit
  [n]
  (if (coprime? 10 n)
    (loop [x 1, k 1]
      (if (zero? (mod x n))
        k
        (recur (mod (inc (* x 10)) n) (inc k))))
    0))

(defn euler-130
  [limit]
  (->> (iterate #(+ 2 %) 5)
       (filter #(coprime? 10 %))
       (remove p/is-prime?) 
       (map #(vector % (repunit %)))
       (filter (fn [[n u]]
                 (zero? (mod (dec n) u))))
       (take limit)
       (map first)
       (apply +)))

(comment
  (time (euler-130 25))
  )