(ns caesarhu.clojure-euler.euler-097
  (:require [caesarhu.math.math-tools :refer [power-mod]]))

(defn euler-097
  []
  (let [m 10000000000]
    (-> (power-mod 2 7830457 m)
        (*' 28433)
        (+ 1)
        (mod m))))

(comment
  (time (euler-097))
  )