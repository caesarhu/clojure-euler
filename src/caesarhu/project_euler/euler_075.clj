(ns caesarhu.project-euler.euler-075
  (:require [clojure.math.numeric-tower :as math]
            [caesarhu.math.polynomial :as poly]))

(defn triangle
  [limit]
  (for [m (range 2 (first (poly/quadratic-root 1 1 (- (/ limit 2)))))
        n (range (if (even? m) 1 2) m 2)
        :when (= (math/gcd m n) 1)
        :let [length (* 2 m (+ m n))]
        :while (<= length limit)]
    (into {} (for [i (range 1 (inc (quot limit length)))]
               [(* length i) 1]))))

(defn euler-075
  [limit]
  (->> (triangle limit)
       (apply merge-with +)
       (filter #(= (val %) 1))
       count))

(comment
  (time (euler-075 1500000))
  (time (triangle 100))
  )