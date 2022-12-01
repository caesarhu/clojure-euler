(ns caesarhu.clojure-euler.euler-026
  (:require [caesarhu.math.cycle-detection :refer [floyd-detection]]))

(defn next-digit
  [[n m]]
  (when-not (zero? n)
    [(mod (* n 10) m) m]))

(defn cycle-length
  [n]
  (if-let [[_ length] (floyd-detection next-digit [1 n])]
    length
    0))

(defn euler-026
  [limit]
  (apply max-key cycle-length (range 2 limit)))

(comment
  (time (euler-026 1000))
  )