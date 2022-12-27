(ns caesarhu.clojure-euler.euler-100
  (:require [clojure.math.numeric-tower :as math]
            [caesarhu.math.pell-equation :refer [pell-solutions]]))

; a(a-1) = 2b(b-1)
; let x=2a-1 , y=2b-1

(defn euler-100
  [start]
  (->> (pell-solutions 2 -1)
       (map (fn [v]
              (map #(quot (+ 1 %) 2) v)))
       (drop-while #(< (first %) start))
       first
       last))

(comment
  (time (euler-100 (math/expt 10 12)))
  )