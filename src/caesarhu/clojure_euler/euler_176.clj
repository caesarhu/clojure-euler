(ns caesarhu.clojure-euler.euler-176
  (:require [caesarhu.math.primes :as p]
            [clojure.math.numeric-tower :refer [expt sqrt]]))

(defn euler-176
  [target]
  (let [sum (inc (* 2 target))
        [x & _ :as prime-expt] (->> (p/factors sum)
                                       (map dec))]
    (->> (map #(expt %1 %2) (p/primes) prime-expt)
         (apply *')
         (#(if (>= x 2) (*' 4 %) %))
         sqrt)))

(comment
  (euler-176 47547)
  )
