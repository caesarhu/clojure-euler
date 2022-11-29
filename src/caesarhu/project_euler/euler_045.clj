(ns caesarhu.project-euler.euler-045
  (:require [caesarhu.math.polynomial :as poly]))

(def hexagonal
  (poly/quadratic 2 -1 0))

(defn pentagonal?
  [c]
  (some pos-int? (poly/quadratic-root 3 -1 (- (* 2 c)))))

(defn triangle?
  [c]
  (some pos-int? (poly/quadratic-root 1 1 (- (* 2 c)))))

(defn euler-045
  []
  (->> (map hexagonal (iterate inc 2))
       (filter pentagonal?)
       (filter triangle?)
       rest
       first))

(comment
  (time (euler-045))
  )