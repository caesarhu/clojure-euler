(ns caesarhu.clojure-euler.euler-190
  (:require [clojure.math.numeric-tower :refer [expt]]))

(defn P
  [m]
  (->> (for [i (range 1 (inc m))
             :let [x (/ (* 2 i) (inc m))]]
         (expt x i))
       (apply *)
       long))

(defn euler-190
  [m]
  (->> (range 2 (inc m))
       (map P)
       (apply +)))

(comment
  (time (euler-190 15))
  )
