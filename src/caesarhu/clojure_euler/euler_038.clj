(ns caesarhu.clojure-euler.euler-038
  (:require [caesarhu.math.math-tools :as tools]))

(defn is-pandigital? [s]
  (= "123456789" (apply str (sort s))))

(defn take-9-digits
  [s]
  (loop [s s
         result []]
    (cond
      (= (count result) 9) result
      (> (count result) 9) nil
      :else (recur (rest s) (concat result (tools/digits (first s)))))))

(defn pandigital-multiples
  [n]
  (let [ds (->> (map #(* n %) (range 1 6))
                take-9-digits)]
    (and (is-pandigital? ds) (tools/digits->number ds))))

(defn euler-038
  []
  (->> (range 9 10000)
       (map pandigital-multiples)
       (remove false?)
       (apply max)))

(comment
  (time (euler-038))
  )