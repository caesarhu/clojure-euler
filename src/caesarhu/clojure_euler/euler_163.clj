(ns caesarhu.clojure-euler.euler-163)

(defn euler-163
  [n]
  (-> (reduce (fn [m i]
                (let [init (+ (* 3 (m (- i 1)))
                              (- (* 3 (m (- i 2))))
                              (m (- i 3))
                              6)
                      value (cond-> (if (zero? (mod i 2))
                                      (+ init 25)
                                      (+ init 12))
                              (not= 1 (mod i 3)) (+ 12)
                              (not= 1 (mod i 4)) (+ 3)
                              (not= 1 (mod i 5)) (#(if (or (= 2 (mod i 5))
                                                           (zero? (mod i 5)))
                                                     (+ % 12)
                                                     (+ % 6))))]
                  (merge m {i value})))
              {0 0 1 16 2 104}
              (range 3 (inc n)))
      (#(% n))))

(comment
  (time (euler-163 36))
  )
