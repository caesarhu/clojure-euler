(ns caesarhu.clojure-euler.euler-004
  (:require [caesarhu.math.math-tools :refer [palindrome?]]
            [caesarhu.kuafu.sat :as sat]))

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

(defn kuafu-004
  []
  (let [model (sat/cp-model)
        a (sat/int-var model 1 9 "a")
        b (sat/int-var model 0 9 "b")
        c (sat/int-var model 0 9 "c")
        p (sat/int-var model 900000 999999 "p")
        x (sat/int-var model 100 999 "x")
        y (sat/int-var model 100 999 "y")
        solver (sat/cp-solver)]
    (sat/add-equality model p (sat/weighted-sum [a b c c b a] [100000 10000 1000 100 10 1]))
    (sat/maximize model p)
    (sat/add-multiplication-equality model p x y)
    (sat/solve solver model)
    (sat/value solver p)))

(comment
  (time (kuafu-004))
  )