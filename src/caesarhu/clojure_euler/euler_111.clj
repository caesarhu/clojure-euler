(ns caesarhu.clojure-euler.euler-111
  (:require [caesarhu.math.primes :as p]
            [clojure.math.combinatorics :as comb]
            [caesarhu.math.math-tools :refer [digits->number]]))

(def problem-digits 10)
(def tail-digits #{1 3 7 9})
(def other-digits (set (range 10)))

(defn invalid-digits?
  [digits]
  (or (zero? (first digits))
      (not (tail-digits (last digits)))
      (zero? (mod (apply + digits) 3))))

(defn other-possible
  [possible-digits n]
  (comb/combinations (apply concat (repeat n (seq possible-digits))) n))

(defn d-combination
  [d n]
  (let [other (other-possible (disj other-digits d) (- problem-digits n))
        ds (repeat n d)]
    (->> (map #(concat % ds) other)
         (mapcat comb/permutations)
         (remove invalid-digits?)
         (map digits->number))))

(defn d-max-repeat
  [d]
  (let [repeat-range (reverse (range 2 problem-digits))]
    (some not-empty (for [r repeat-range]
                      (for [p (d-combination d r)
                            :when (p/is-prime? p)]
                        p)))))

(defn max-repeat
  []
  (mapcat d-max-repeat (range 10)))

(defn euler-111
  []
  (apply + (max-repeat)))

(comment
  (time (euler-111))
  )