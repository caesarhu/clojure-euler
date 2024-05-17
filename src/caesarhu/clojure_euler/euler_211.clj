(ns caesarhu.clojure-euler.euler-211
  (:require [clojure.math.numeric-tower :refer [expt sqrt]]
            [caesarhu.math.primes :as p]
            [clojure.core.reducers :as r]))

(defn is-square?
  [^long n]
  (let [sqr (sqrt n)]
    (= (* sqr sqr) n)))

(defn brute-force
  [^long limit]
  (let [prime-seq (p/primes limit)
        prime-set (set prime-seq)
        factors (fn [^long n] (frequencies (p/factors n prime-seq)))
        divisor-square-sum (fn [^long n]
                             (->> (for [[p e] (factors n)]
                                    (->> (for [i (range 1 (inc e))]
                                           (if (= i 1)
                                             (inc (* p p))
                                             (expt p (* i 2))))
                                         (apply +)))
                                  (apply *)))]
    (inc (r/fold + (r/filter #(is-square? (divisor-square-sum %)) (r/remove prime-set (range 2 limit)))))))

(comment
  (time (brute-force 64000000))
  )

(defn prime-sigma2
  [^long prime, ^long e]
  (r/fold + (for [i (range 1 (inc e))]
              (if (= i 1)
                (inc (* prime prime))
                (expt prime (* i 2))))))

(defn euler-211
  [^long limit]
  (let [v (reduce (fn [v p]
                    (if-not (= (v p) 1) v
                            (loop [i (+ p p)
                                   s (rest (p/power-seq (dec limit) p))
                                   nv v]
                              (if (empty? s) nv
                                  (recur (+ i p) (next s) (update nv i * (prime-sigma2 p (first s))))))))
                  (vec (repeat limit 1))
                  (range 2 limit))]
    (inc (r/fold + (for [i (range 2 limit)
                         :let [sigma2 (v i)]
                         :when (and (not= sigma2 1) (is-square? sigma2))]
                     i)))))

(comment
  (time (euler-211 64000000))
  )
