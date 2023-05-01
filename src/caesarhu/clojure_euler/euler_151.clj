(ns caesarhu.clojure-euler.euler-151
  (:require [clojure.math :refer [round]]))

(defn expand
  [papers n]
  (let [p (nth papers n)]
    (->> (concat (take n papers)
                 [(dec p)]
                 (map inc (drop (inc n) papers)))
         (drop-while #(zero? %)))))

(defn expand-papers
  [[prob papers]]
  (when (> (count papers) 1)
    (let [sum (reduce + papers)]
      (for [i (range (count papers))
            :let [c (nth papers i)]
            :when (pos? c)]
        [(* prob (/ c sum)) (expand papers i)]))))

(defn euler-151
  [papers]
  (loop [result []
         prob-papers [[1 papers]]]
    (if (empty? prob-papers)
      (->> (map first result)
           (apply +)
           (* 1000000)
           round
           (* 1/1000000)
           double)
      (recur (concat result (filter (fn [[_ papers]]
                                      (and (= 1 (reduce + papers))
                                           (not= 1 (count papers))))
                                    prob-papers))
             (apply concat (map expand-papers prob-papers))))))

(comment
  (time (euler-151 [1 1 1 1]))
  )