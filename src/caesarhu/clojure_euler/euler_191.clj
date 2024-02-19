(ns caesarhu.clojure-euler.euler-191
  (:require [clojure.math.combinatorics :as c]))

(def states (set (range 3)))
(def init-map {[0 0] [1 0]
               [0 1] [0 1]
               [0 2] [1 0]}) ; 0 on time, 1 late, 2 absent; key stores last t digits

(defn next-state
  [m state]
  (->> (for [[[d1 d2] [v1 v2]] m
             :when (not= 2 state d1 d2)]
         (if (= 1 state)
           {[d2 state] [0 v1]}
           {[d2 state] [v1 v2]}))
       (apply merge-with #(vec (map + %1 %2)))))

(defn next-map
  [m]
  (->> (map #(next-state m %) states)
       (apply merge-with #(vec (map + %1 %2)))))

(defn map-sum
  [m]
  (->> (vals m)
       (map #(apply + %))
       (apply +)))

(defn euler-191
  [day]
  (-> (iterate next-map init-map)
      (nth (dec day))
      map-sum))

(comment
  (time (euler-191 30))
  )
