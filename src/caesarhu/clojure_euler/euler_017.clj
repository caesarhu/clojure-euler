(ns caesarhu.clojure-euler.euler-017
  (:require [caesarhu.math.math-tools :refer [digits]]))

(defn to-units [n]
  (let [rdig (reverse (digits n))]
    (->> (reductions * 1 (repeat 10))
         (map * rdig)
         (filter pos?)
         reverse)))

(defn say [n]
  (let [units (to-units n)
        words {0 nil
               1 "one"
               2 "two"
               3 "three"
               4 "four"
               5 "five"
               6 "six"
               7 "seven"
               8 "eight"
               9 "nine"
               10 "ten"
               11 "eleven"
               12 "twelve"
               13 "thirteen"
               14 "fourteen"
               15 "fifteen"
               16 "sixteen"
               17 "seventeen"
               18 "eighteen"
               19 "nineteen"
               20 "twenty"
               30 "thirty"
               40 "forty"
               50 "fifty"
               60 "sixty"
               70 "seventy"
               80 "eighty"
               90 "ninety"}]
    (loop [x (first units)
           xs (rest units)
           out nil]
      (cond
        (nil? x) out
        (and (= x 10) (seq xs)) (concat out [(words (+ x (first xs)))])
        (contains? words x) (recur (first xs) (rest xs) (concat out [(words x)]))
        (and (>= x 100) (<= x 900))
        (if (seq xs)
          (recur (first xs) (rest xs) (concat out [(words (/ x 100)) "hundred" "and"]))
          (recur (first xs) (rest xs) (concat out [(words (/ x 100)) "hundred"])))
        (>= x 1000) (recur (first xs) (rest xs) (concat out [(words (/ x 1000)) "thousand"]))))))


(defn count-letters [n]
  (reduce + (map count (say n))))

(defn euler-017 []
  (reduce + (map count-letters (range 1 1001))))

(comment
  (time (euler-017))
  )