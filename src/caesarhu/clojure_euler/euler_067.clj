(ns caesarhu.clojure-euler.euler-067
  (:require [caesarhu.clojure-euler.euler-018 :refer [euler-018]]))

(def fname "resources/p067_triangle.txt")

(defn get-triangle []
  (letfn [(parse-nums [s] (map #(Integer/parseInt %) (clojure.string/split s #"\W")))]
    (->> (map parse-nums (clojure.string/split-lines (slurp fname)))
         (map vec)
         vec)))

(comment
  (time (euler-018 (get-triangle)))
  )