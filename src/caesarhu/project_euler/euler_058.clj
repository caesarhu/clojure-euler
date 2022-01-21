(ns caesarhu.project-euler.euler-058
  (:require [caesarhu.math.primes :as p]))

(defn corners
  [n]
  (if (= n 1)
    [1]
    (let [n2 (* n n)
          l (dec n)]
      (take 4 (iterate #(- % l) n2)))))

(defn euler-058
  [target]
  (reduce (fn [[primes nums] n]
            (let [ns (+ nums 4)
                  ps (->> (corners n) (filter p/is-prime?) count (+ primes))]
              (if (> target (/ ps ns))
                (reduced n)
                [ps ns]))) 
          [0 1] (iterate #(+ % 2) 3)))

(comment
  (time (euler-058 1/10))
  )