(ns caesarhu.clojure-euler.euler-134
  (:require [caesarhu.math.chinese-remainder :refer [crt]]
            [clojure.math.numeric-tower :refer [expt]]
            [caesarhu.math.math-tools :refer [digits]]
            [caesarhu.math.primes :as p]))

(defn pair-connection
  [[p1 p2]]
  (let [d10 (->> (digits p1) count (expt 10))]
    (crt [p2 d10] [0 p1])))

(defn euler-134
  [limit]
  (->> (drop-while #(< % 5) (p/primes))
       (partition 2 1)
       (take-while #(< (first %) limit))
       (map pair-connection)
       (apply +)))

(comment 
  (time (euler-134 1000000))
  )