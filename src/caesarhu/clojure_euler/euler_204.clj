(ns caesarhu.clojure-euler.euler-204
  (:require [caesarhu.math.primes :as p]
            [clojure.math.numeric-tower :refer [expt]]
            [caesarhu.math.math-tools :refer [digits]]))

(defn *prime
  [limit n p]
  (for [e (range)
        :let [pe (expt p e)
              npe (* n pe)]
        :while (<= npe limit)]
    npe))

(defn make-hamming-numbers
  [limit n]
  (let [prime-vec (p/primes (inc n))]
    (loop [ns [1]
           ps prime-vec]
      (if (empty? ps)
        ns
        (let [p (first ps)]
          (recur (mapcat #(*prime limit % p) ns) (rest ps)))))))

(defn euler-204-slow
  [limit n]
  (count (make-hamming-numbers limit n)))

(comment
  (time (euler-204-slow (expt 10 8) 5))
  )

(def primes (atom (vec (p/primes 1000))))

(defn log2
  [x]
  (count (digits x 2)))

(defn count-hamming-numbers
  [x j]
  (let [pj (@primes j)]
    (cond
      (= 1 x) 1
      (zero? j) (log2 x)
      (< x pj) (recur x (dec j))
      :else (+ (count-hamming-numbers x (dec j))
               (count-hamming-numbers (quot x pj) j)))))

(defn euler-204
  [limit n]
  (let [target (->> (p/primes (inc n)) count dec)]
    (count-hamming-numbers limit target)))

(comment
  (time (euler-204 (expt 10 9) 100))
  )
