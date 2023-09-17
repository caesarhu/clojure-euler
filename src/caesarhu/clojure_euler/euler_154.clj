(ns caesarhu.clojure-euler.euler-154
  (:require [caesarhu.math.math-tools :refer [factorial]]
            [clojure.math.numeric-tower :refer [expt]]
            [caesarhu.math.primes :as p]
            [clojure.core.reducers :as r]))

(def << bit-shift-left)
(def >> bit-shift-right)

(defn euler-154-kpark
  [^long m]
  (let [f2 (loop [i 1
                  v (vector 0)]
             (if (> i m) v
                 (recur (inc i) (assoc v i (+ (>> i 1) (v (>> i 1)))))))
        f5 (loop [i 1
                  v (vector 0)]
             (if (> i m) v
                 (recur (inc i) (assoc v i (+ (quot i 5) (v (quot i 5)))))))
        horiz-count (fn [^long n ^long d2 ^long d5]
                      (let [count (loop [j 1
                                         c 0]
                                    (if (>= (<< j 1) n) c
                                        (recur (inc j)
                                               (if (and (>= d2 (+ (f2 j) (f2 (- n j))))
                                                        (>= d5 (+ (f5 j) (f5 (- n j)))))
                                                 (+ c 2)
                                                 c))))]
                        (if (and (zero? (bit-and n 1))
                                 (>= d2 (<< (f2 (>> n 1)) 1))
                                 (>= d5 (<< (f5 (>> n 1)) 1)))
                          (inc count)
                          count)))]
    (loop [total 0
           i 0]
      (if (> i m)
        total
        (let [c2 (- (f2 m) (+ (f2 (- m i)) (f2 i) 12))
              c5 (- (f5 m) (+ (f5 (- m i)) (f5 i) 12))]
          (if (and (nat-int? c2) (nat-int? c5))
            (recur (+ total (* i (>> (inc i) 1))) (inc i))
            (recur (+ total (horiz-count i (+ c2 (f2 i)) (+ c5 (f5 i)))) (inc i))))))))

(defn euler-154-slow
  [^long m]
  (let [f2 (loop [i 1
                  v (vector 0)]
             (if (> i m) v
                 (recur (inc i) (assoc v i (+ (>> i 1) (v (>> i 1)))))))
        f5 (loop [i 1
                  v (vector 0)]
             (if (> i m) v
                 (recur (inc i) (assoc v i (+ (quot i 5) (v (quot i 5)))))))
        count-legal (fn [^long x ^long y ^long z]
                      (if (and (>= (- (f5 m) (f5 x) (f5 y) (f5 z)) 12)
                               (>= (- (f2 m) (f2 x) (f2 y) (f2 z)) 12))
                        1 0))]
    (transduce (filter pos?)
               +
               (for [x (range (inc m))
                     y (range (inc (- m x)))
                     :let [z (- m x y)]]
                 (count-legal x y z)))))

(defn euler-154
  [^long max]
  (let [f2 (loop [i 1
                  v (vector 0)]
             (if (> i max) v
                 (recur (inc i) (assoc v i (+ (>> i 1) (v (>> i 1)))))))
        f5 (loop [i 1
                  v (vector 0)]
             (if (> i max) v
                 (recur (inc i) (assoc v i (+ (quot i 5) (v (quot i 5)))))))
        x-max (quot max 3)
        m2 (- (f2 max) 12)
        m5 (- (f5 max) 12)]
    (r/fold + (for [x (range (inc x-max))
                    :let [y-max (- max x)
                          x2 (- m2 (f2 x))
                          x5 (- m5 (f5 x))
                          y-max-half (quot (inc y-max) 2)
                          zz (- y-max x)]]
                (+ (if (and (zero? (mod x 2)) (<= (* 2 (f5 y-max-half)) x5) (<= (* 2 (f2 y-max-half)) x2))
                     3 0)
                   (if (and (<= (+ (f5 x) (f5 zz)) x5) (<= (+ (f2 x) (f2 zz)) x2))
                     3 0)
                   (r/fold +
                           (for [y (range (inc x) y-max-half)
                                 :let [z (- y-max y)]
                                 :when (and (<= (+ (f5 y) (f5 z)) x5) (<= (+ (f2 y) (f2 z)) x2))]
                             6)))))))

(comment
  (time (euler-154-kpark 200000))
  (time (euler-154 200000))
  )
