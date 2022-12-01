(ns caesarhu.clojure-euler.euler-062
  (:require [caesarhu.math.math-tools :as tools]))

(defn sorted-digits
  [n]
  (sort (tools/digits n)))

(defn cube-map
  [limit]
  (->> (map #(* % % %) (range 1 limit))
       (map #(hash-map (sorted-digits %) [%]))
       (apply merge-with concat)))

(defn euler-062
  [limit]
  (->> (cube-map limit)
       vals
       (filter #(>= (count %) 5))
       (map first)
       (apply min)))

(comment
  (time (euler-062 10000))
  )