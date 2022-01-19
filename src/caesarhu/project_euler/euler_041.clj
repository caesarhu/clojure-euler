(ns caesarhu.project-euler.euler-041
  (:require [caesarhu.math.math-tools :as tools]
            [caesarhu.math.primes :as p]
            [clojure.math.combinatorics :as combo]))

(defn pandigital-digits
  [n]
  (reverse (range 1 (inc n))))

(defn max-pandigital-prime
  [n]
  (->> (combo/permutations (pandigital-digits n))
       (map tools/digits->number)
       (some #(and (p/is-prime? %) %))))

(defn euler-041
  []
  (max-pandigital-prime 7))

(comment
  (time (euler-041))
  "因為 2、3、5、6、8、9位數 pandigital 一定可以被3整除，所以只有4、7位數有可能有 pandigital 質數")