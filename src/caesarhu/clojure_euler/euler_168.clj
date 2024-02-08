(ns caesarhu.clojure-euler.euler-168
  (:require [clojure.math.numeric-tower :refer [expt]]))

(defn euler-168
  [dn]
  (->> (for [n (range 2 (inc dn))
             :let [neunen (dec (expt 10 n))]
             d (range 1 10)
             p (range d (* 10 d))
             :when (and (= 9 (mod p 10))
                        (zero? (mod (* neunen d) p)))]
         (quot (* neunen d) p))
       (apply +)
       (#(mod % (expt 10 5)))
       int))

(comment
  (time (euler-168 100))
  )
