(ns caesarhu.clojure-euler.euler-200
  (:require [caesarhu.math.primes :as p]
            [caesarhu.math.math-tools :refer [digits digits->number]]
            [clojure.math.numeric-tower :refer [expt]]))

(defn is-200?
  [n]
  (re-find #"200" (str n)))

(defn prime-proof?
  [n]
  (let [test-n #{1 3 7 9}
        n-1 (vec (butlast (digits n)))]
    (->> (for [i test-n]
           (digits->number (conj n-1 i)))
         (every? #(not (p/is-prime? %))))))

(defn candidate-pn
  [p e]
  (let [primes (->> (p/primes) (drop 3) (cons 3))
        take-n (if (and (= p 2) (= e 3))
                 (partial take 200)
                 (partial take 50))]
    (->> (map #(*' (expt p e) (expt % (- 5 e))) primes)
         (filter is-200?)
         (filter prime-proof?)
         take-n)))

(defn candidate
  []
  (let [c23 (candidate-pn 2 3)
        c22 (candidate-pn 2 2)
        c53 (candidate-pn 5 3)
        c52 (candidate-pn 5 2)]
    (->> (concat c23 c22 c53 c52)
         (cons 200)
         sort)))

(defn euler-200
  [n]
  (nth (candidate) (dec n)))

(comment
  (time (euler-200 200))
  )
