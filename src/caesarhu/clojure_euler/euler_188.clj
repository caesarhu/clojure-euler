(ns caesarhu.clojure-euler.euler-188
  (:require [caesarhu.math.math-tools :refer [power-mod]]
            [clojure.math.numeric-tower :refer [expt]]
            [caesarhu.math.primes :as p]))

(defn hyperexponentiation
  [b e m]
  (loop [i 1
         result 1]
    (if (> i e) result
        (recur (inc i) (power-mod b result m)))))

(defn euler-188
  [b e]
  (let [p (expt 10 8)
        m (p/totient p)
        power (hyperexponentiation b e m)]
    (power-mod b power p)))

(comment
  (time (euler-188 1777 1855))
  )
