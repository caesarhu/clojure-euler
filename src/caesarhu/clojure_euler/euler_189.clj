(ns caesarhu.clojure-euler.euler-189
  (:require [clojure.math.combinatorics :as c]))

(def colors #{0 1 2}) ; 0 :red 1 :green 2 :blue
(def row-init {[0] 1, [1] 1, [2] 1})

(defn exclude
  [& more]
  (apply disj colors more))

(defn flip
  [row]
  (->> (for [[k v] row
             target (->> (map exclude k)
                         (apply c/cartesian-product)
                         (map vec))]
         {target v})
       (apply merge-with +)))

(defn row-extend
  [row]
  (let [flip-row (flip row)]
    (->> (for [[k v] flip-row
               color colors
               :let [new-key (conj k color)]]
           {new-key (->> (for [source (partition 2 1 new-key)]
                           (apply exclude source))
                         (apply c/cartesian-product)
                         (map flip-row)
                         (apply +))})
         (apply merge))))

(defn euler-189
  [n]
  (loop [i (dec n)
         row row-init]
    (if (zero? i)
      (->> (map val row)
           (apply +))
      (recur (dec i) (row-extend row)))))

(comment
  (time (euler-189 8))
  )
