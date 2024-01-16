(ns caesarhu.clojure-euler.euler-155
  (:require [clojure.set :as set]))

(defn dot
  [s1 s2]
  (transduce (comp (map (fn [x] [x (/ x)]))
                   (map set))
             set/union
             #{}
             (for [a s1
                   b s2]
               (+ a b))))

(defn next-set
  [s]
  (let [n (inc (count s))]
    (reduce set/union
            #{}
            (for [i (range (quot n 2))]
              (dot (nth s i) (nth s (- n i 2)))))))

(defn euler-155
  [n]
  (loop [s [#{1}]
         all (apply set/union s)]
    (if (< (count s) n)
      (let [ns (next-set s)
            new-all (set/union all ns)]
        (recur (conj s (set/difference ns all)) new-all))
      (count all))))

(comment
  (time (euler-155 10))
  )
