(ns caesarhu.project-euler.euler-080
  (:require [clojure.math.numeric-tower :as math]
            [caesarhu.math.math-tools :as tools]))

(defn newton-method
  [n]
  (letfn [(newton [x]
                  (/ (+ x (/ n x)) 2))]
    (iterate newton (rationalize (math/sqrt n)))))

(defn newton-100
  [n]
  (if (tools/square? n) 0
      (->> (nth (newton-method n) 3)
           (* (math/expt 10 100))
           tools/digits
           (take 100)
           (apply +))))

(defn euler-080
  []
  (->> (range 1 101)
       (map newton-100)
       (apply +)))

(comment
  (time (euler-080))
  )