(ns caesarhu.clojure-euler.euler-171
  (:require [caesarhu.math.math-tools :refer [digits]]
            [clojure.math.numeric-tower :refer [expt exact-integer-sqrt]]
            [clojure.math.combinatorics :as c]))

(defn square
  [n]
  (* n n))

(def square? #(->> (exact-integer-sqrt %) last zero?))

(defn sum-map
  [d]
  (loop [i 1
         sm (into {} (map #(vector (square %) [1 %]) (range 10)))]
    (if (= i d)
      (filter #(square? (key %)) sm)
      (recur (inc i)
             (->> (for [x (range 10)
                        [k [n sum]] sm]
                    {(+' (square x) k) [n (+' sum (*' (expt 10 i) x n))]})
                  (apply merge-with #(map +' %1 %2)))))))

(defn euler-171
  [d]
  (let [sm (sum-map d)]
    (->> (vals sm)
         (map last)
         (apply +')
         (#(mod % (expt 10 9)))
         int)))

(comment
  (sum-map 20)
  (time (euler-171 20))
  )
