(ns caesarhu.clojure-euler.euler-117)

(defn next-permutations
  [v]
  (->> (conj v (apply + v)) (take-last 4) vec))

(def permutations-seq
  (map last (iterate next-permutations [1])))

(defn euler-117
  [n]
  (nth permutations-seq n))

(comment
  (time (euler-117 50))
  )