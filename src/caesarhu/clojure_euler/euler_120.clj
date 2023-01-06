(ns caesarhu.clojure-euler.euler-120)

(defn max-remainder
  [a]
  (- (* a a)
     (if (even? a)
       (+ a a)
       a)))

(defn euler-120
  [limit]
  (->> (range 3 (inc limit))
       (map max-remainder)
       (apply +)))

(comment
  (time (euler-120 1000))
  )