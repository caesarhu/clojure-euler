(ns caesarhu.clojure-euler.euler-060
  (:require [caesarhu.math.primes :as p]
            [caesarhu.math.math-tools :as tools]
            [clojure.math.combinatorics :as combo]))

(defn is-pair?
  [p1 p2]
  (and (pos-int? (mod (+ p1 p2) 3))
       (let [d1 (tools/digits p1)
             d2 (tools/digits p2)]
         (->> [(concat d1 d2) (concat d2 d1)]
              (map tools/digits->number)
              (every? p/is-prime?)))))

(defn next-map
  [m]
  (->> (for [[k v] m
             p v
             :let [pk (conj k p)]
             :let [ps (filter #(is-pair? p %) v)]]
         {(conj k p) ps})
       (apply merge)))

(defn euler-060
  [limit n]
  (loop [m {#{} (p/primes limit)}
         n n]
    (if (zero? n)
      (->> (keys m) (map #(apply + %)) (apply min))
      (recur (next-map m) (dec n)))))

(comment
  (time (euler-060 8500 5))
  )
