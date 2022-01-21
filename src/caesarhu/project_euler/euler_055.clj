(ns caesarhu.project-euler.euler-055
  (:require [caesarhu.math.math-tools :as tools]))

(defn reverse-number
  [n]
  (->> (tools/digits n)
       (reverse)
       tools/digits->number))

(defn reverse-sum
  [n]
  (drop 1 (iterate #(+ % (reverse-number %)) (bigint n))))

(def lychrel-limit 50)

(defn lychrel-number?
  [n]
  (->> (take lychrel-limit (reverse-sum n))
       (some tools/palindrome?)
       not))

(defn euler-055
  [limit]
  (->> (range 1 limit)
       (filter lychrel-number?)
       count))

(comment
  (time (euler-055 10000))
  )