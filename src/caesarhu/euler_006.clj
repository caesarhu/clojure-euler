(ns caesarhu.euler-006)

(defn sum-of-numbers
  [n]
  (quot (* n (inc n)) 2))

(defn sum-of-squares
  [n]
  (quot (* n (inc n) (inc (* n 2))) 6))

(defn euler-006
  [n]
  (- (* (sum-of-numbers n) (sum-of-numbers n))
     (sum-of-squares n)))

(comment
  (time (euler-006 100))
  )