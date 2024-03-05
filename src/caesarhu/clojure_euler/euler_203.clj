(ns caesarhu.clojure-euler.euler-203
  (:require [caesarhu.math.primes :as p]))

(defn next-row
  [row]
  (let [head (cons 0 row)
        tail (concat row [0])]
    (map + head tail)))

(defn pascal-triangle
  [n]
  (->> (iterate next-row [1])
       (take n)))

(defn squarefree?
  [n]
  (->> (p/factors n)
       (apply distinct?)))

(defn brute-force
  [n]
  (->> (pascal-triangle n)
       (apply concat)
       set
       (filter squarefree?)
       (apply +)))

(comment
  (time (brute-force 51))
  )
