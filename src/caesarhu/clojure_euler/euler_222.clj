(ns caesarhu.clojure-euler.euler-222
  (:require [clojure.math.numeric-tower :refer [sqrt round]]))

(def R 50)

(defn euler-222
  [n]
  (let [r-map (->> (for [i (range n)
                         :let [idx (if (even? i)
                                     (quot i 2)
                                     (- n 1 (quot i 2)))]]
                     [idx (- R i)])
                   (into (sorted-map)))
        calc (fn [i] (* 2 (sqrt (* R (+ (r-map i) (r-map (inc i)) (- R))))))]
    (->> (reduce (fn [sum i]
                   (+ sum (calc i)))
                 (+ (r-map 0) (r-map (dec n)))
                 (range (dec n)))
         (* 1000)
         round)))

(comment
  (euler-222 21)
  )
