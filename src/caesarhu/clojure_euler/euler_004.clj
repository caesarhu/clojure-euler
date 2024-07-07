(ns caesarhu.clojure-euler.euler-004
  (:require [caesarhu.math.math-tools :refer [palindrome?]]))

(def start 999)
(def end 100)

(defn euler-004
  []
  (->> (for [i (range start end -1)
             :let [dj (if (zero? (mod i 11)) 1 11)
                   j-start (if (zero? (mod i 11)) i
                             (* 11 (quot i 11)))]]
         (first (for [j (range j-start end (- dj))
                      :when (palindrome? (* i j))]
                  (* i j))))
       (remove nil?)
       (apply max)))

(comment
  (time (euler-004))
  )
