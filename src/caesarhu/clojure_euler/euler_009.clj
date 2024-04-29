(ns caesarhu.clojure-euler.euler-009
  (:require [caesarhu.math.math-tools :refer [pythagorean-triplet]]))

(defn euler-009
  [^long n]
  (let [target? (fn [s] (zero? (mod n (apply + s))))
        target (->> (filter target? (pythagorean-triplet))
                    first)
        times (quot n (apply + target))]
    (apply * times times times target)))

(comment
  (euler-009 1000)
  )
