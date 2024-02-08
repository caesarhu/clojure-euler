(ns caesarhu.clojure-euler.euler-170
  (:require [clojure.math.numeric-tower :refer [gcd]]
            [caesarhu.math.math-tools :refer [digits digits->number]]
            [clojure.math.combinatorics :as c]))

(defn pandigital?
  [& ns]
  (let [s (mapcat digits ns)]
    (and (= 10 (count s))
         (apply distinct? s))))

(defn split-2group
  [s]
  (for [i (range 1 10)]
    (split-at i s)))

(defn brute-force
  []
  (let [p (c/permutations (->> (range 10) reverse))]
    (->> (mapcat split-2group p)
         (remove #(or (zero? (first (second %)))
                      (zero? (ffirst %))))
         (map (fn [ss] (map digits->number ss)))
         (filter #(zero? (mod (apply gcd %) 3)))
         (filter (fn [ns]
                   (let [g (apply gcd ns)
                         [a b] (map #(quot % g) ns)]
                     (pandigital? g a b)))))))

(defn euler-170
  []
  (->> (brute-force) first))

(comment
  (time (euler-170))
  )
