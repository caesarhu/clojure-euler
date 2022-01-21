(ns caesarhu.project-euler.euler-057
  (:require [caesarhu.math.math-tools :as tools]))

(def root-fraction-2
  (iterate (fn [[a b]]
             (let [new-a (+' a b)]
               [new-a (+' a new-a)])) [2 3]))

(defn euler-057
  [limit]
  (->> (take limit root-fraction-2)
       (map (fn [xs] (map #(count (tools/digits %)) xs)))
       (filter #(apply < %))
       count))

(comment
  (euler-057 1000)
  )