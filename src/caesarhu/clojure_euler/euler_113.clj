(ns caesarhu.clojure-euler.euler-113)

(def init-vector
  (let [init (vec (repeat 10 (vec (repeat 10 0))))]
    (reduce (fn [acc i]
              (assoc-in acc [i i] 1))
            init (range 0 10))))

(defn next-vector
  [v]
  (let [next-value (fn [[i j]]
                     (apply + (-> v (nth i) (subvec i (inc j)))))]
    (reduce (fn [acc [i j]]
              (-> acc 
                  (assoc-in [i j] (next-value [i j]))
                  (assoc-in [j i] (next-value [i j]))))
            v (for [i (range 10)
                    j (range i 10)]
                [i j]))))

(defn euler-113
  [expt]
  (let [vs (take expt (iterate next-vector init-vector))]
    (->> (map #(drop 1 %) vs)
         flatten
         (apply +))))

(comment
  (time (euler-113 6))
  )