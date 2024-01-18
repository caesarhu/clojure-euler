(ns caesarhu.clojure-euler.euler-158
  (:require [clojure.math.combinatorics :as combo]
            [clojure.math.numeric-tower :refer [expt]]))

(defn small?
  [a b]
  (if (< a b) 1 0))

(defn one?
  [s]
  (= 1 (->> (partition 2 1 s)
            (map #(apply small? %))
            (apply +))))

(defn p
  [n]
  (* (combo/count-combinations (range 26) n)
     (- (expt 2 n) n 1)))

(defn euler-158
  []
  (->> (range 2 27)
       (map p)
       (apply max)))

(comment
  (p 3)
  (time (euler-158))
  )
