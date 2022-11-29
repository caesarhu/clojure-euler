(ns caesarhu.project-euler.euler-036
  (:require [caesarhu.math.math-tools :as tools]))

(defn double-palindromes?
  [n]
  (and (tools/palindrome? n)
       (let [ds (tools/digits n 2)]
         (= ds (reverse ds)))))

(defn euler-036
  [limit]
  (->> (range 1 limit)
       (filter double-palindromes?)
       (apply +)))

(comment
  (time (euler-036 1000000))
  )