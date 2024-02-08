(ns caesarhu.clojure-euler.euler-164
  (:require [caesarhu.math.math-tools :refer [digits]]
            [clojure.math.numeric-tower :refer [expt]]
            [clojure.set :as s]))

(def max-sum 9)

(defn valid?
  [n]
  (if (< n 100)
    (->> (digits n)
         (apply +)
         (>= max-sum))
    (->> (digits n)
         (partition 3 1)
         (map #(apply + %))
         (every? #(<= % max-sum)))))

(defn brute-force
  [n]
  (let [start (expt 10 (dec n))
        limit (expt 10 n)]
    (->> (range start limit)
         (filter valid?)
         count)))

(def init-map
  (frequencies (map #(vector % 0) (range 10))))

(defn next-map
  [m]
  (->> (for [i (range 10)
             [[a b] n] m
             :when (<= (+ i a b) 9)]
         {[i a] n})
       (apply merge-with +')))

(defn count-map
  [m]
  (apply +' (for [[[a _] n] m
                 :when (not (zero? a))]
             n)))

(defn euler-164
  [digits]
  (loop [d 1
         m init-map]
    (if (>= d digits)
      (count-map m)
      (recur (inc d) (next-map m)))))

(comment
  init-map
  (time (euler-164 20))
  )
