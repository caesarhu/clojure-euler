(ns caesarhu.project-euler.euler-070
  (:require [clojure.math.numeric-tower :as math]
            [clojure.math.combinatorics :as combo]
            [caesarhu.math.math-tools :refer [digits]]
            [caesarhu.math.primes :as p]))

(defn sort-digits
  [n]
  (sort (digits n)))

(defn totient
  [a b]
  (* a b (/ (dec a) a) (/ (dec b) b)))

(defn permutation?
  [a b]
  (->> [(* a b) (totient a b)]
       (map sort-digits)
       (apply =)))

(defn euler-070
  [limit]
  (let [sq (math/sqrt limit)
        low-primes (reverse (p/primes sq))
        hi-primes (take (* (count low-primes) 2) (p/primes))]
    (->> (combo/combinations hi-primes 2)
         (filter #(< (apply * %) limit))
         (filter #(apply permutation? %))
         (apply min-key #(/ (apply * %) (apply totient %)))
         (apply *))))

(comment
  (time (euler-070 10000000))
  )