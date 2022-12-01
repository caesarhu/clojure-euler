(ns caesarhu.clojure-euler.euler-074
  (:require [caesarhu.math.math-tools :as tools]
            [caesarhu.math.cycle-detection :refer [floyd-detection]]))

(defn factorial-sum
  [n]
  (->> (tools/digits n)
       (map tools/factorial)
       (apply +)))

(defn sort-digits
  [n]
  (sort (tools/digits n)))

(def target (sort-digits 367945))

(defn equal-60
  [n]
  (= target (sort-digits (factorial-sum n))))

(defn euler-074
  [limit]
  (->> (range 11 limit)
       (filter equal-60)
       count))

(comment
  (time (euler-074 1000000))
  )