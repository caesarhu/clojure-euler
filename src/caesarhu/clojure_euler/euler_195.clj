(ns caesarhu.clojure-euler.euler-195)

(defn count-factor
  [limit]
  (reduce (fn [v1 i]
            (if (= 1 (v1 i))
              (reduce (fn [v2 j] (update v2 j * 2))
                      v1
                      (range i (inc limit) i))
              v1))
          (vec (repeat (inc limit) 1))
          (range 2 (inc limit))))

(defn euler-195
  [limit]
  (let [upper-bound-1 (long (* limit 2 (/ (Math/sqrt 3))))
        upper-bound-2 (long (* limit 6 (/ (Math/sqrt 3))))
        factor-count (count-factor upper-bound-2)]
    (->> (for [i (range 2 (inc upper-bound-2))]
           (if (= 1 (mod i 3))
             (* (quot (factor-count i) 2) (quot upper-bound-2 i))
             (* (quot (factor-count i) 2) (quot upper-bound-1 i))))
         (apply +))))

(comment
  (->> (count-factor 100)
       (map #(vector %1 %2) (range 101)))
  (time (euler-195 1053779))
  )
