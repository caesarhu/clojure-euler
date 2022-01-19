(ns caesarhu.project-euler.euler-045
  (:require [caesarhu.math.polynomial :as poly]))

(def hexagonal
  (poly/quadratic 2 -1 0))

(defn pentagonal?
  [c]
  (poly/quadratic-root-pred? pos-int? 3/2 -1/2 (- (/ c 2))))