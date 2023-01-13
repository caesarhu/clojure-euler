(ns caesarhu.clojure-euler.euler-126)

(defn cubes
  [x y z n]
  (+ (* 2 (+ (* x y) (* y z) (* x z)))
     (* 4 (+ x y z) (dec n))
     (* 4 (- n 2) (dec n))))

(defn cubes-range
  ([limit]
   (take-while #(< (cubes % % % 1) limit) (rest (range))))
  ([limit x]
   (take-while #(< (cubes x x % 1) limit) (iterate inc x)))
  ([limit x y]
   (take-while #(< (cubes x y % 1) limit) (iterate inc y)))
  ([limit x y z]
   (take-while #(< (cubes x y z %) limit) (iterate inc 1))))

(defn euler-126
  [target]
  (let [limit (* target 21)
        counter (atom (vec (repeat limit 0)))
        inc-counter (fn [c]
                      (let [i (nth @counter c)]
                        (swap! counter assoc c (inc i))))]
    (doseq [x (cubes-range limit)
            y (cubes-range limit x)
            z (cubes-range limit x y)
            n (cubes-range limit x y z)
            :let [cand (cubes x y z n)]]
      (inc-counter cand))
    (some #(and (= target (nth @counter %)) %) (range))))

(comment
  (time (euler-126 1000))
  )