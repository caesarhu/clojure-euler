(ns caesarhu.clojure-euler.euler-150
  (:require [clojure.math.numeric-tower :refer [expt]]))

(def sk-vector
  (let [base (expt 2 20)]
    (->> (iterate #(mod (+ (* 615949 %) 797807) base) 0)
         rest
         (map #(- % (expt 2 19))))))

(defn sk-triangle
  [rows]
  (->> (reduce (fn [[acc sk] i]
                 (let [[head tail] (split-at i sk)]
                   [(conj acc (vec head)) tail]))
               [[] sk-vector] (range 1 (inc rows)))
       first))

(defn euler-150
  [rows]
  (let [triangle (->> (sk-triangle rows) reverse)
        init-row (vec (repeat (inc rows) [(expt 10 8) (expt 10 8)]))]
    (reduce (fn [acc v])
            init-row triangle)))

(comment
  (euler-150 10)
  )