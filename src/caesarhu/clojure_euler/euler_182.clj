(ns caesarhu.clojure-euler.euler-182
  (:require [clojure.math.numeric-tower :refer [gcd]]))

(defn phi
  [p q]
  (* (dec p) (dec q)))

(defn euler-182
  [p q]
  (let [n (* p q)
        phi (phi p q)
        es (filter #(= 1 (gcd % phi)) (range 3 phi 2))
        minimal? (fn [e] (and (= 1 (gcd e phi)) (= 2 (gcd (dec e) phi))))]
    (->> es
         (filter minimal?)
         (apply +))))

(comment
  (time (euler-182 1009 3643))
  )
