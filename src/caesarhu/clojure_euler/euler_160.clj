(ns caesarhu.clojure-euler.euler-160
  (:require [caesarhu.math.math-tools :refer [power-mod digits]]
            [clojure.math.numeric-tower :refer [expt gcd]]
            [caesarhu.math.primes :as p]))

(defn legendre
  [n p]
  (quot (- n (apply + (digits n p)))
        (dec p)))

(defn euler-160
  [n modulo]
  (let [mod* (fn [& more] (reduce #(mod (* %1 %2) modulo) more))
        init (apply mod* 1 (->> (range 1 (inc modulo) 2) (remove #(zero? (mod % 5)))))
        e2 (- (legendre n 2) (legendre n 5))
        e2r (power-mod 2 e2 modulo)
        mod-other (fn [n init]
                    (apply mod* init (->> (range 1 (inc (mod n modulo)) 2) (remove #(zero? (mod % 5))))))
        fx (fn [n p]
             (loop [n n
                    v 1]
               (if (< n 2) v
                   (recur (quot n p) (mod* v (mod-other n init))))))
        f2 (fn [n]
             (loop [n n
                    v 1]
               (if (< n 2) v
                   (recur (quot n 2) (mod* v (mod-other n init) (fx (quot n 5) 5))))))]
    (mod* e2r (f2 n))))

(comment
  (time (euler-160 (expt 10 12) 100000))
  )
