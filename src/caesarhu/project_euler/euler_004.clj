(ns caesarhu.project-euler.euler-004
  (:require [caesarhu.math.math-tools :refer [palindrome?]]))

(defn euler-004
  "palindrome number is divisible by 11."
  []
  (loop [answer 0
         a 999]
    (let [db (if (zero? (mod a 11)) 1 11)
          new-answer (loop [b (if (zero? (mod a 11)) 999 990)]
                       (let [a*b (* a b)]
                         (when (and (>= b a)
                                    (> a*b answer))
                           (if (palindrome? a*b)
                             a*b
                             (recur (- b db))))))]
      (cond
        (< a 100) answer
        new-answer (recur new-answer (dec a))
        :else (recur answer (dec a))))))

(comment
  (time (euler-004))
  )