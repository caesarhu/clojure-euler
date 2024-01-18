(ns caesarhu.clojure-euler.euler-159
  (:require [clojure.math.numeric-tower :refer [expt]]))

(defn drs
  [n]
  (inc (mod (dec n) 9)))

(defn euler-159
  [n]
  (let [mdrs (atom (vec (repeat n 0)))]
    (doseq [i (range 2 n)]
      (swap! mdrs assoc i (max (@mdrs i) (drs i)))
      (doseq [j (range 2 (inc i))
              :while (< (* i j) n)]
        (swap! mdrs assoc (* i j) (max (@mdrs (* i j)) (+ (@mdrs j) (@mdrs i))))))
    (apply + @mdrs)))

(comment
  (drs 467)
  (time (euler-159 (expt 10 6)))
  )
