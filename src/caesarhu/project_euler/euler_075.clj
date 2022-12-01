(ns caesarhu.project-euler.euler-075
  (:require [caesarhu.math.math-tools :as tools]))

(defn make-length-map
  [limit]
  (->> (tools/pythagorean-triplet)
       (map #(apply + %)) 
       (take-while #(<= % (* 2 limit)))
       (filter #(<= % limit))
       (mapcat (fn [L]
                 (map #(* L %) (range 1 (inc (quot limit L))))))
       frequencies))

(defn euler-075
  [limit]
  (->> (make-length-map 1500000)
       (filter #(= (val %) 1))
       count))

(comment
  (time (euler-075 1500000))
  )