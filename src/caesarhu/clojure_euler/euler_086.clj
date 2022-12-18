(ns caesarhu.clojure-euler.euler-086)

(defn square?
  [n]
  (let [sqrt (int (Math/sqrt n))]
    (= n (* sqrt sqrt))))

(defn cuboids 
  [m]
  (reduce
   +
   (for [a (filter #(square? (+ (* % %) (* m m)))
                   (range 1 (inc (* 2 m))))]
     (if (> a m)
       (- (quot a 2) (- a m 1))
       (quot a 2)))))

(defn euler-086 
  [L]
  (count (take-while #(> L %) (reductions + (map cuboids (range))))))

(comment
  (time (euler-086 1000000))
  )