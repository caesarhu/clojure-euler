(ns caesarhu.clojure-euler.euler-123
  (:require [caesarhu.math.primes :as p]
            [clojure.math.numeric-tower :refer [expt]]))

(defn euler-123
  [limit]
  (->> (map vector (iterate inc 1) (p/primes))
       (filter #(odd? (first %)))
       (drop-while #(<= (apply * 2 %) (expt 10 10)))
       first))

(comment
  (time (euler-123 1000))
  )