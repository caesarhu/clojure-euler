(ns caesarhu.clojure-euler.euler-039
  (:require [caesarhu.math.math-tools :as tools]))

(defn *limit
  [limit n]
  (->> (map #(* n %) (drop 1 (range)))
       (take-while #(<= % limit))))

(defn euler-039
  [limit]
  (->> (map #(apply + %) (tools/pythagorean-triplet))
       (take-while #(<= % 1000))
       (mapcat #(*limit limit %))
       frequencies
       (apply max-key val)
       first))

(comment
  (time (euler-039 1000))
  )