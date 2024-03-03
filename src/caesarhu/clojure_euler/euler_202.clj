(ns caesarhu.clojure-euler.euler-202
  (:require [clojure.math.numeric-tower :refer [gcd]]
            [caesarhu.math.primes :as p]))

(defn euler-202-slow
  [N]
  (let [n (quot (+ N 3) 2)
        offset (mod (* 2 n) 3)]
    (->> (for [k (range 0 (+ (quot (- (quot n 2) offset) 3) 1))
               :when (= 1 (gcd n (+ (* 3 k) offset)))]
           1)
         (apply +)
         (* 2))))

(defn euler202-helper
  [z p-vec]
  (let [[d-vec m-vec] (reduce (fn [[d-vec m-vec] p]
                                [(into d-vec (map #(* p %) d-vec))
                                 (into m-vec (map - m-vec))])
                              [[1] [1]]
                              p-vec)]
    (->> (for [i (range (count d-vec))
               :let [x (quot z (d-vec i))
                     t (+ (quot (dec x) 3) (if (= 2 (mod x 3)) 1 0))]]
           (* (m-vec i) t))
         (apply +))))

(defn euler-202
  [n]
  (when (odd? n)
    (let [z (quot (+ n 3) 2)
          p-vec (->> (p/factors z) distinct)]
      (euler202-helper z p-vec))))

(comment
  (time (euler-202-slow 12017639147))
  (time (euler-202 12017639147))
  )
