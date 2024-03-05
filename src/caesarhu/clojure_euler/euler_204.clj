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

(def prime-vec (vec (p/primes 1000)))

(defn log2
  [x]
  (count (digits x 2)))

(defn count-hamming-numbers
  [x j]
  (let [pj (prime-vec j)]
    (cond
      (= 1 x) 1
      (zero? j) (log2 x)
      (< x pj) (recur x (dec j))
      :else (+ (count-hamming-numbers x (dec j))
               (count-hamming-numbers (quot x pj) j)))))

(defn euler-204-recursive
  [limit n]
  (let [target (->> (p/primes (inc n)) count dec)]
    (count-hamming-numbers limit target)))

(defn euler-204
  [limit n]
  (let [p-vec (vec (p/primes (inc n)))]
    (loop [hamming-numbers 0
           h-list [[limit (dec (count p-vec))]]]
      (if (empty? h-list)
        hamming-numbers
        (let [[x j] (first h-list)
              pj (p-vec j)]
          (cond
            (= 1 x) (recur (inc hamming-numbers) (rest h-list))
            (zero? j) (recur (+ (log2 x) hamming-numbers) (rest h-list))
            (< x pj) (recur hamming-numbers (cons [x (dec j)] (rest h-list)))
            :else (recur hamming-numbers (into (rest h-list) [[(quot x pj) j] [x (dec j)]]))))))))

(comment
  (time (euler-204 (expt 10 18) 100))
  )
