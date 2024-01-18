(ns caesarhu.clojure-euler.euler-160
  (:require [caesarhu.math.math-tools :refer [power-mod factorial]]
            [clojure.math.numeric-tower :refer [expt gcd]]
            [caesarhu.math.primes :as p]))

(defn factorial-exp
  [n p]
  (->> (map #(expt p %) (iterate inc 1))
       (take-while #(<= % n))
       (map #(quot n %))
       (apply +)))

(defn modulo-without25
  [modulo]
  (reduce (fn [v n]
            (mod (* v n) modulo))
          1
          (->> (iterate #(+ 2 %) 3)
               (take-while #(< % modulo))
               (remove #(zero? (mod % 5))))))

(defn g
  [n modulo modulo-without25-modulo]
  (let [n (mod n modulo)]
    (reduce (fn [v n]
              (mod (* v n) modulo))
            modulo-without25-modulo
            (->> (iterate #(+ 2 %) 3)
                 (take-while #(< % (inc n)))
                 (remove #(zero? (mod % 5)))))))

(defn h
  [n modulo modulo-without25-modulo]
  (if (< n 2) 1
      (mod (* (g n modulo modulo-without25-modulo) (h (quot n 5) modulo modulo-without25-modulo)) modulo)))

(defn f
  [n modulo modulo-without25-modulo]
  (if (< n 2) 1
      (mod (* (g n modulo modulo-without25-modulo) (f (quot n 2) modulo modulo-without25-modulo) (h (quot n 5) modulo modulo-without25-modulo)) modulo)))

(defn euler-160
  [n modulo]
  (let [modulo-without25-modulo (power-mod (modulo-without25 modulo) (quot n modulo) modulo)
        x (factorial-exp n 2)
        y (factorial-exp n 5)
        s (power-mod 2 (- x y) modulo)]
    (mod (* s (f n modulo modulo-without25-modulo)) modulo)))

(comment
  (p/totient (expt 10 5))
  (time (euler-160 (expt 10 12) 100000))
  )
