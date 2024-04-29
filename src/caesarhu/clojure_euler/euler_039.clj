(ns caesarhu.clojure-euler.euler-039
  (:require [caesarhu.math.math-tools :as tools :refer [pythagorean-triplet]]))

(defn euler-039
  [^long limit]
  (let [triplets (pythagorean-triplet (fn [s] (<= (apply + s) limit)))
        times (fn [perimeter]
                (->> (iterate (partial + perimeter) perimeter)
                     (take-while #(<= % limit))))
        perimeter-map (->> triplets
                           (map #(apply + %))
                           (mapcat times)
                           frequencies)]
    (-> (apply max-key val perimeter-map)
        key)))

(comment
  (euler-039 1000)
  )
