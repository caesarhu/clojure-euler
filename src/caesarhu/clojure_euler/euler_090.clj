(ns caesarhu.clojure-euler.euler-090
  (:require [clojure.set :as set]
            [caesarhu.math.math-tools :refer [digits]]
            [clojure.math.combinatorics :as combo]))

(def set-69 #{6 9})
(def square-sets (->> (map #(* % %) (range 1 10))
                      (map digits)
                      (mapcat #(if (= 1 (count %)) (cons 0 %) %))
                      (map #(if (set-69 %) set-69 #{%}))
                      (partition 2)
                      (map set)
                      set))

(defn valid?
  [square-sets s1 s2]
  (let [match? (fn [s1 s2 square-set]
                 (->> (map #(set/intersection %1 %2) [s1 s2] square-set)
                      (every? not-empty)))
        match-square? (fn [s1 s2 square-set]
                        (or (match? s1 s2 square-set) (match? s2 s1 square-set)))]
    (every? #(match-square? s1 s2 %) square-sets)))

(defn brute-force
  []
  (let [s (combo/combinations (range 10) 6)]
    (->> (combo/cartesian-product s s)
         (filter #(apply valid? square-sets (map set %)))
         (map set)
         set
         count)))

(defn add-pair
  [p1 p2]
  (let [pair (cond
               (empty? p1) [#{} #{}]
               (= 1 (count p1)) [(first p1) (first p1)]
               :else (seq p1))]
    (->> (apply combo/cartesian-product p2)
         (mapcat #(vector % (reverse %)))
         (map (fn [v] (map conj pair v)))
         (map set)
         set)))

(defn square-pairs
  [square-sets]
  (reduce (fn [acc pair]
            (->> (map #(add-pair % pair) acc)
                 (apply set/union)))
          [#{}] square-sets))

(defn fill-digits
  [s]
  (let [length (- 6 (count s))
        diff-set (set/difference (set (range 10)) s)]
    (if (zero? length) [s]
        (->> (combo/combinations (seq diff-set) length)
             (map #(set/union s (set %)))))))

(defn fill-pair
  [pair]
  (let [[s1 s2] (seq pair)]
    (->> (combo/cartesian-product (fill-digits s1) (fill-digits s2))
         (map set)
         set)))

(defn euler-090
  []
  (->> (square-pairs square-sets)
       (filter (fn [v]
                 (every? #(>= 6 (count %)) v)))
       (map fill-pair)
       (apply set/union)
       count))

(comment
  (time (brute-force))
  (combo/combinations (range 10) 6)
  (time (euler-090))
  )