(ns caesarhu.project-euler.euler-060
  (:require [caesarhu.math.primes :as p]
            [caesarhu.math.math-tools :as tools]))

(def concatenated-primes
  (memoize
   (fn [n limit]
     (let [d (tools/digits n)]
       (set
        (for [a (p/primes-range 2 limit)
              :when (and
                     (p/is-prime? (tools/digits->number (concat d (tools/digits a))))
                     (p/is-prime? (tools/digits->number (concat (tools/digits a) d))))]
          a))))))

(defn subset [limit & ks]
  (let [s (apply clojure.set/intersection (map #(concatenated-primes % limit) ks))]
    (apply (partial disj s) ks)))

(defn solve [limit]
  (first
   (for [a (p/primes-range 2 limit)
         b (subset limit a)
         c (subset limit a b)
         d (subset limit a b c)
         e (subset limit a b c d)
         :let [s [a b c d e]]]
     {:sum (reduce + s) :primes s})))

(comment
  (time (solve 10000))
  )