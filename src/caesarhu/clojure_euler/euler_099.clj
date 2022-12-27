(ns caesarhu.clojure-euler.euler-099
  (:require [clojure.math :as math]
            [clojure.string :as str]))

(def base-exp "resources/p099_base_exp.txt")

(defn get-numbers
  [fname]
  (->> (str/split-lines (slurp fname))
       (map #(str/split % #","))
       (map (fn [coll]
              (map #(Long/parseLong %) coll)))))

(defn log-value
  [[base exp]]
  (*' exp (math/log base)))

(defn euler-099
  [filepath]
  (->> (get-numbers filepath)
       (map #(vector %1 (log-value %2)) (iterate inc 1))
       (apply max-key last)))

(comment
  (euler-099 base-exp)
  )