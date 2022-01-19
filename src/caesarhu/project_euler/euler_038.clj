(ns caesarhu.project-euler.euler-038
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
  (let [ds (->> (map #(* n %) (range 1 10))
                take-9-digits)]
    (and (is-pandigital? ds) (tools/digits->number ds))))

(defn solve
  [n]
  (->> (range 2 n)
       (map pandigital-multiples)
       (remove false?)
       (apply max)))

(comment
  (time (solve 10000))
  )