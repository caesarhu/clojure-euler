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
  (loop [result 0
         prob-papers [[1 papers]]]
    (if (empty? prob-papers)
      (->> (* 1000000 result)
           round
           (* 1/1000000)
           double)
      (let [[f & other] prob-papers
            current (expand-papers f)]
        (recur (apply + result (->> (filter (fn [[_ papers]]
                                              (and (= 1 (reduce + papers))
                                                   (not= 1 (count papers))))
                                            current)
                                    (map first)))
               (concat current other))))))

(comment
  (time (euler-151 [1 0 0 0 0]))
  )