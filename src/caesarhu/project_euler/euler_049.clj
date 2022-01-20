(ns caesarhu.project-euler.euler-049
  (:require [clojure.math.combinatorics :as combo]
            [caesarhu.math.primes :as p]
            [caesarhu.math.math-tools :as tools]))

(defn ->digits
  [n]
  {(sort (tools/digits n)) [n]})

(defn arithmetic?
  [coll]
  (->> (combo/combinations coll 3)
       (filter (fn [[a b c]]
                 (= (+ b b) (+ a c))))))

(defn euler-049
  []
  (->> (p/primes-range 1000 10000)
       (map ->digits)
       (apply merge-with concat)
       (map last)
       (filter #(>= (count %) 3))
       (map arithmetic?)
       (remove empty?)
       (apply concat)
       (map #(apply str %))
       (map #(Long/parseLong %))))

(comment
  (time (euler-049))
  )