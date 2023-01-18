(ns caesarhu.clojure-euler.euler-132
  (:require [caesarhu.math.primes :as p]
            [clojure.math.numeric-tower :refer [expt sqrt]]
            [caesarhu.math.math-tools :refer [coprime? power-mod]]))

(defn repunit
  [n]
  (if (coprime? 10 n)
   (loop [x 1, k 1]
     (if (zero? (mod x n))
       k
       (recur (mod (inc (* x 10)) n) (inc k))))
    0))

(defn repunit-factors
  [n]
  (for [p (drop-while #(<= % 5) (p/primes))
        :let [u (repunit p)]
        :when (zero? (mod n u))]
    p))

(defn brute-force
  [target n]
  (->> (repunit-factors target)
       (take n)
       (apply +)))

(defn factor?
  [n p]
  (let [phi (fn [p] (dec p))
        mn (mod n (phi p))]
    (= 1 (power-mod 10 mn (* 9 p)))))

(defn euler-132
  [target n]
  (->> (drop-while #(<= % 5) (p/primes))
       (filter #(factor? target %))
       (take n)
       (apply +)))

(comment
  (time (euler-132 (expt 10 9) 40))
  )