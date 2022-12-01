(ns caesarhu.clojure-euler.euler-071)

(defn euler-071
  [target limit]
  (when (ratio? target)
    (let [n (numerator target)
          d (denominator target)
          target-ratio (fn [x]
                         (let [xd (quot (* x n) d)]
                           (if (= (* xd d) (* n x))
                             (/ (dec xd) x)
                             (/ xd x))))
          start (max 2 (long (* d (dec (quot limit 7)))))]
      (->> (range start (inc limit))
           (map target-ratio)
           (apply max)
           numerator))))

(comment
  (time (euler-071 3/7 1000000))
  )