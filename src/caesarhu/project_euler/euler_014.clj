(ns caesarhu.project-euler.euler-014)

(defn euler-014
  [limit]
  (let [max-index (atom 1)
        steps (atom [0 0])
        count-length (fn [i]
                       (loop [num i
                              length 0]
                         (cond
                           (= num 1) length
                           (< num i) (+ length (@steps num))
                           (even? num) (recur (quot num 2) (inc length))
                           :else (recur (inc (* 3 num)) (inc length)))))]
    (doseq [i (range 2 limit)]
      (let [length (count-length i)]
        (swap! steps assoc i length)
        (when (> length (@steps @max-index))
          (reset! max-index i))))
    @max-index))

(defn solve-014
  [limit]
  (let [count-length (fn [steps i]
                       (loop [num i
                              length 0]
                         (cond
                           (= num 1) length
                           (< num i) (+ length (steps num))
                           (even? num) (recur (quot num 2) (inc length))
                           :else (recur (inc (* 3 num)) (inc length)))))]
    (loop [i 2
           max-index 1
           steps [0 0]]
      (if (= i limit) max-index
          (let [length (count-length steps i)]
            (recur (inc i)
                   (if (> length (steps max-index)) i max-index)
                   (assoc steps i length)))))))

(comment
  (time (euler-014 1000000))
  (time (solve-014 1000000))
  )
