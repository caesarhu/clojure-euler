(ns caesarhu.clojure-euler.euler-037
  (:require [caesarhu.math.math-tools :as tools]
            [caesarhu.math.primes :as p]))

(defn truncatable-prime?
  [n]
  (let [trun (fn [f v] (map #(f % v) (range 1 (count v))))
        digits (tools/digits n)]
    (->> (concat [digits] (trun take digits) (trun take-last digits))
         (map tools/digits->number)
         (every? p/is-prime?))))

(defn euler-037
  [n]
  (->> (drop 4 (p/primes))
       (filter truncatable-prime?)
       (take (min 11 n))
       (apply +)))

(comment
  (time (euler-037 11))
  )