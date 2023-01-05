(ns caesarhu.clojure-euler.euler-115)

(defn make-seq
  [m]
  (let [v (repeat (inc m) 1)
        next (fn [v]
               (let [rv (reverse v)
                     n (+ (* 2 (nth rv 0)) (- (nth rv 1)) (nth rv m))]
                 (->> (cons n rv) (take (inc m)) reverse)))]
    (rest (map first (iterate next v)))))

(defn euler-115
  [limit]
  (->> (map vector (range) (make-seq 50))
       (drop-while #(<= (last %) limit))
       first))

(comment
  (time (euler-115 1000000))
  )