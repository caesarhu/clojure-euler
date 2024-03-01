(ns caesarhu.clojure-euler.euler-201)

(defn sum-squares
  [n]
  (quot (* n (inc n) (inc (* 2 n))) 6))

(def sum-limit (-> (sum-squares 50) (* 1.06) long))

(defn sum-recursive
  [start current-sum k sum-map]
  (let [new-sum (+ current-sum (* start start))]
    (cond
      (zero? k) (merge-with + sum-map {current-sum 1})
      (> new-sum sum-limit) sum-map
      :else (let [new-sum-map (sum-recursive (inc start) new-sum (dec k) sum-map)]
              (if (= sum-map new-sum-map)
                sum-map
                (recur (inc start) current-sum k new-sum-map))))))

(defn euler-201-guess
  []
  (let [n 100
        k (quot n 2)
        count-sum (->> (sum-recursive 1 0 k {})
                       (filter #(= 1 (last %)))
                       count)]
    (* (sum-squares 100) count-sum)))

(defn euler-201
  [x]
  (let [target (quot x 2)
        min+ (fn [c1 c2] (min 2 (+ c1 c2)))
        get-knc (fn [knc j k]
                  (map (fn [[n c]] [(inc k) (+ n (* (inc j) (inc j))) c]) (knc k)))]
    (loop [j 0
           knc (vec (cons {0 1} (repeat target {})))]
      (if (>= j x)
        (let [target-count (->> (knc target) (filter #(= 1 (last %))) count)]
          (-> target-count (quot 2) (* (sum-squares x))))
        (let [to-add (mapcat #(get-knc knc j %) (range target))]
          (recur (inc j) (reduce (fn [v [k n c]]
                                   (update v k #(merge-with min+ % {n c})))
                                 knc
                                 to-add)))))))

(comment
  (time (euler-201 60))
  )
