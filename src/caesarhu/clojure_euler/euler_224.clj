(ns caesarhu.clojure-euler.euler-224
  (:require [clojure.math.numeric-tower :refer [sqrt]]
            [caesarhu.math.math-tools :refer [square?]]))

(def target 75000000)

(defn square-sum
  [^long n]
  (let [x (dec (* n n))]
    (for [a (iterate inc 1)
          :let [a2 (* a a)]
          :while (<= a2 (quot x 2))
          :let [b2 (- x a2)]
          :when (square? b2)]
      [a (sqrt b2) n])))

(defn brute-force
  [^long limit]
  (->> (range 2 limit)
       (mapcat square-sum)))

(def pattern
  [[[-2, 1, 2], [-1, 2, 2], [-2, 2, 3]],
   [[2, 1, 2], [1, 2, 2], [2, 2, 3]],
   [[1, -2, 2], [2, -1, 2], [2, -2, 3]]])

(defn next-triangles
  [[^long a, ^long b, ^long c]]
  (map (fn [s] (map #(->> (map * % [a b c]) (apply +)) s))
       (if (= a b) (rest pattern) pattern)))

(defn euler-224
  [^long limit]
  (loop [triangles [[2 2 3]]
         answer 1]
    (if (empty? triangles)
      answer
      (let [new-triangles (->> (next-triangles (first triangles))
                               (filter #(<= (apply + %) limit)))]
        (recur (concat new-triangles (rest triangles))
               (+ answer (count new-triangles)))))))

(comment
  (time (euler-224 target))
  )
