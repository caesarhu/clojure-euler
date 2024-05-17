(ns caesarhu.clojure-euler.euler-223
  (:require [caesarhu.math.primes :as p :refer [range-factors]]
            [clojure.math.numeric-tower :refer [expt]]))

(def target 25000000)

(defn almost-triangles
  [factors-map, ^long limit, ^long a]
  (cond
    (= a 1) (quot (dec limit) 2)
    (= a 2) 0
    :else (let [power-seq (fn [^long n ^long power]
                            (for [i (range (inc power))]
                              (expt n i)))
                x (* (inc a) (dec a))
                m (merge-with + (factors-map (inc a)) (factors-map (dec a)))
                divisors (->> (map #(apply power-seq %) m)
                              (reduce p/product-coll)
                              sort)]
            (->> (for [d (take (quot (count divisors) 2) divisors)
                       :let [dx (quot x d)
                             y (+ d dx)
                             c (quot y 2)
                             b (- c d)]
                       :while (>= b a)
                       :when (and (even? (+ d dx))
                                  (> (+ a b) c)
                                  (<= (+ a b c) limit))]
                   1)
                 (apply +)))))

(defn euler-223-slow
  [^long limit]
  (let [factors-map (range-factors (+ 2 (quot limit 3)))]
    (->> (range 1 (quot limit 3))
         (map #(almost-triangles factors-map limit %))
         (apply +))))

(comment
  (time (euler-223-slow target))
  )

; https://projecteuler.net/thread=223;page=5 lightln2's solution
(defn next-almost-triangles
  [[^long a, ^long b, ^long c]]
  (let [a1 (+ a (- b) c)
        b1 (+ a (quot (+ b c) 2))
        c1 (+ a (quot (- (* 3 c) b) 2))
        a2 (+ a b c)
        b2 (+ a (quot (+ (- b) c) 2))
        c2 (+ a (quot (+ (* 3 c) b) 2))]
    [[a1 b1 c1] [a2 b2 c2]]))

(defn euler-223
  [^long limit]
  (let [illegal? (fn [[a b c]] (and (> a b) (odd? c)))
        count-almost-triangles (fn [s] (count (remove illegal? s)))
        accept? (fn [s] (<= (apply + s) limit))]
    (loop [triangles [[1 1 1]]
           sum 1]
      (if (empty? triangles)
        sum
        (let [new-triangles (->> (next-almost-triangles (first triangles))
                                 (filter accept?))]
          (recur (concat new-triangles (rest triangles))
                 (+ sum (count-almost-triangles new-triangles))))))))

(comment
  (time (euler-223 target))
  )
