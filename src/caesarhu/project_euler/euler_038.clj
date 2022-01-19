(ns caesarhu.project-euler.euler-038
  (:require [caesarhu.math.math-tools :as tools]))

(defn is-pandigital? [s]
  (= "123456789" (apply str (sort s))))

(defn pandigital-multiples
  [n]
  (if-let [result (not-empty (for [i (range 2 10)
                                   :let [products (->> (take i (range 1 10))
                                                       (map #(* n %))
                                                       (apply str))]
                                   :when (is-pandigital? products)]
                               products))]
    (bigint (first result))
    0))

(defn solve
  [n]
  (->> (range 1 n)
       (map pandigital-multiples)
       (apply max)))

(comment
  (time (solve 100000))
  )