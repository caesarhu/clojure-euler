(ns caesarhu.project-euler.euler-060
  (:require [caesarhu.math.primes :as p]
            [caesarhu.math.math-tools :as tools]
            [clojure.core.memoize :as m]))

(def concatenated-primes
  (m/lu
   (fn [n limit]
     (let [d (tools/digits n)]
       (set
        (for [a (p/primes-range 2 limit)
              :when (and
                     (p/is-prime? (tools/digits->number (concat d (tools/digits a))))
                     (p/is-prime? (tools/digits->number (concat (tools/digits a) d))))]
          a))))
   :lu/threshold 10000))

(defn subset 
  [limit & ks]
  (let [s (apply clojure.set/intersection (map #(concatenated-primes % limit) ks))]
    (apply (partial disj s) ks)))

(defn euler-060 
  [limit]
  (first
   (for [a (p/primes-range 2 limit)
         b (subset limit a)
         c (subset limit a b)
         d (subset limit a b c)
         e (subset limit a b c d)
         :let [s [a b c d e]]]
     {:sum (reduce + s) :primes s})))

(comment
  (time (euler-060 10000))
  )

(defn concatenated?
  [p1 p2]
  (let [d1 (tools/digits p1)
        d2 (tools/digits p2)]
    (->> [(concat d1 d2) (concat d2 d1)]
         (map tools/digits->number)
         (every? p/is-prime?))))

(def limit-primes (p/primes-range 10000))

(comment
  (filter #(concatenated? 13 %) limit-primes)
  )