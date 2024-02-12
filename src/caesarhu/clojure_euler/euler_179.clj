(ns caesarhu.clojure-euler.euler-179)

(defn euler-179
  [limit]
  (let [divisors (atom (vec (repeat limit 2)))
        result (atom 0)]
    (doseq [i (range 2 (Math/sqrt limit))
            :let [square-i (* i i)]]
      (swap! divisors assoc square-i (inc (@divisors square-i)))
      (doseq [j (range (+ square-i i) limit i)]
        (swap! divisors assoc j (+ 2 (@divisors j)))))
    (doseq  [i (range 2 (dec limit))]
      (when (= (@divisors i) (@divisors (inc i)))
        (swap! result inc)))
    @result))

(comment
  (time (euler-179 10000000))
  )
