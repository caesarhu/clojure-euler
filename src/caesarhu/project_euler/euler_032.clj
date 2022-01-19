(ns caesarhu.project-euler.euler-032
  (:require [caesarhu.math.math-tools :as tools]
            [clojure.math.combinatorics :as combo]))

(defn is-pandigital? 
  [xs]
  (= "123456789" (->> (apply str xs)
                      sort
                      (apply str))))

(defn brute-force []
  (reduce +
          (distinct
           (for [a (range 2 100)
                 b (range (inc a) (/ 9999 a))
                 :when (is-pandigital? (str a b (* a b)))]
             (* a b)))))

(comment
  (time (brute-force))
  )