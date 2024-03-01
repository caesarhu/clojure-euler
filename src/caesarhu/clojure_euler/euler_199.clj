(ns caesarhu.clojure-euler.euler-199
  (:require [clojure.math.numeric-tower :refer [round expt]]))

(defn inner-circle
  [k1 k2 k3]
  (let [x2 (+ (* k1 k1) (* k2 k2) (* k3 k3))
        x1 (+ k1 k2 k3)
        b (* -2 x1)
        c (- (* 2 x2) (* x1 x1))]
    (/ (+ (- b) (Math/sqrt (- (* b b) (* 4 c)))) 2)))

(defn euler-199
  [iterations]
  (let [k0 (/ (+ 2 (Math/sqrt 3)) (Math/sqrt 3))
        area-origin (- 1 (/ 3 (* k0 k0)))]
    (loop [i 0
           current-list [[k0,k0,-1] [k0,-1,k0] [-1,k0,k0] [k0,k0,k0]]
           area-rem 0]
      (if (>= i iterations)
        (-> (- area-origin area-rem) (* (expt 10 8)) round (* (expt 10 -8)) double)
        (let [pair (for [[k1 k2 k3] current-list
                         :let [k (inner-circle k1 k2 k3)
                               area-k (/ 1 (* k k))]]
                     (list area-k (list [k1 k3 k] [k1 k2 k] [k2 k3 k])))
              area (->> (map first pair) (apply +))
              new-list (mapcat last pair)]
          (recur (inc i) new-list (+ area-rem area)))))))

(comment
  (time (euler-199 10))
  )
