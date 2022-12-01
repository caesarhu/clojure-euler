(ns caesarhu.clojure-euler.euler-034
  (:require [caesarhu.math.math-tools :as tools]))

(defn digit-factorials
  [n]
  (->> (tools/digits n)
       (map tools/factorial)
       (apply +)))

(defn curious?
  [n]
  (= n (digit-factorials n)))

(defn euler-034
  []
  (->> (range 10 100000)
       (filter curious?)
       (apply +)))

(comment
  (time (euler-034))
  )

