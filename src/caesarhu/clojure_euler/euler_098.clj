(ns caesarhu.clojure-euler.euler-098
  (:require [caesarhu.math.math-tools :as tools]
            [clojure.string :as str]
            [clojure.math.numeric-tower :as math]))

(def file-098 "resources/p098_words.txt")
(def square-set
  (->> (map #(* % %) (iterate inc 1))
       (take-while #(< % (math/expt 10 9)))
       set))

(defn strip-quotes [s]
  (apply str (filter #(not= % \") s)))

(defn get-data [fname]
  (map
   strip-quotes
   (str/split (slurp fname) #",")))

(defn anagrams [fname]
  (->> (get-data fname)
       (map #(hash-map (sort %) [%]))
       (apply merge-with concat)
       (filter #(> (count (val %)) 1))
       (map val)))

(defn square-digits
  [n]
  (filter #(= n (count (tools/digits %))) square-set))

(defn pair-mapping
  [[a b :as pair] n]
  (let [n-digits (tools/digits n)
        m (zipmap (seq a) n-digits)
        xa (tools/digits->number (replace m a))
        xb (tools/digits->number (replace m b))]
    (when (and (= xa n) 
               (= (count n-digits) (count (tools/digits xb)) (count (distinct (vals m))))
               (square-set xb))
      [xa xb])))

(defn pair-square
  [pair]
  (let [length (count (first pair))
        squares (square-digits length)]
    (->> (map #(pair-mapping pair %) squares)
         (filter some?))))

(defn euler-098
  [filepath]
  (->> (anagrams filepath)
       (map pair-square)
       flatten
       (apply max)))

(comment
  (time (euler-098 file-098))
  (pair-square ["CARE" "RACE"])
  )