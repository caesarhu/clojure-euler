(ns caesarhu.clojure-euler.euler-160
  (:require [caesarhu.math.math-tools :refer [power-mod digits]]
            [clojure.math.numeric-tower :refer [expt gcd]]
            [caesarhu.math.primes :as p]))

(defn legendre
  [n p]
  (quot (- n (apply + (digits n p)))
        (dec p)))

(defn modulo*
  [modulo & more]
  (reduce (fn [v i]
            (mod (* v i) modulo))
          more))

(defn modulo-other
  [modulo n init]
  (apply modulo* modulo init
         (->> (range 1 (inc (mod n modulo)) 2) (remove #(zero? (mod % 5))))))

(defn euler-160
  [n modulo]
  (let [init (apply modulo* modulo 1 (->> (range 1 (inc modulo) 2) (remove #(zero? (mod % 5)))))
        e2 (- (legendre n 2) (legendre n 5))
        e2r (power-mod 2 e2 modulo)]
    (letfn [(f5 [n]
                (if (< n 2) 1
                    (modulo* modulo (modulo-other modulo n init) (f5 (quot n 5)))))
            (f2 [n]
                (if (< n 2) 1
                    (modulo* modulo
                             (modulo-other modulo n init)
                             (f2 (quot n 2))
                             (f5 (quot n 5)))))]
      (modulo* modulo e2r (f2 n)))))

(comment
  (time (euler-160 (expt 10 12) 100000))
  )
