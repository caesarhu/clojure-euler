(ns caesarhu.clojure-euler.euler-215
  (:require [clojure.math.combinatorics :as c]
            [com.rpl.specter :as s]))

(def bricks [2 3])

(defn row-bricks
  [width]
  (let [[x y] bricks]
    (for [i (range (inc (quot width x)))
          :let [new-width (- width (* x i))]
          :when (zero? (mod new-width y))]
      [[i x] [(quot new-width y) y]])))

(defn permutations-bricks
  [bricks]
  (->> (mapcat #(apply repeat %) bricks) c/permutations))

(defn gen-bricks-fn
  ([bricks]
   (fn [row]
     (let [m (->> (mapcat #(apply repeat %) bricks) frequencies)]
       (= (frequencies row) m))))
  ([bricks & more]
   (let [bricks-v (conj more bricks)
         bricks-m (map (fn [bricks]
                         (->> (mapcat #(apply repeat %) bricks) frequencies)) bricks-v)]
     (fn [row]
       (let [m (frequencies row)]
         (some #(= m %) bricks-m))))))

(defn gen-next-rows
  [row]
  (let [width (apply + row)
        gaps (set (reductions + (butlast row)))]
    (loop [next-rows (list [])
           result (list)]
      (if (empty? next-rows)
        result
        (let [new-next-rows (->> (mapcat (fn [row] (map #(conj row %) bricks)) next-rows)
                                 (remove #(> (apply + %) width))
                                 (remove #(gaps (apply + %))))
              matched (filter #(= width (apply + %)) new-next-rows)]
          (recur new-next-rows (concat result matched)))))))

(defn euler-215
  [width high]
  (let [bricks-v (row-bricks width)
        origin-rows (mapcat permutations-bricks bricks-v)
        crack-free? (apply gen-bricks-fn bricks-v)]
    (if (= high 1)
      (count origin-rows)
      (let [origin-map (->> origin-rows
                           (mapcat #(map vector (repeat %) (->> (gen-next-rows %) (filter crack-free?))))
                           (group-by first)
                           (s/transform [s/MAP-VALS s/ALL] last))]
        (loop [level 2
               level-map (s/transform [s/MAP-VALS] count origin-map)]
          (if (>= level high)
            (->> (vals level-map) (apply +))
            (recur (inc level)
                   (->> (for [[row lower-rows] origin-map
                              :let [n (level-map row)]
                              lower-row lower-rows]
                          {lower-row n})
                        (apply merge-with +)))))))))

(comment
  (time (euler-215 32 10))
  )

; https://projecteuler.net/thread=215;page=8 phyn's solution
(defn euler-215-phyn
  [width high]
  (let [states (atom {(vec (repeat high 0)) 1})
        option_transform {0 [1 2] 1 [0] 2 [1]}]
    (doseq [pos (range (- width 2))
            :let [new-states (atom {})]]
      (doseq [[state, combinations] @states
              :let [options (vec (for [x state] (option_transform x)))]]
        (doseq [new-state (apply c/cartesian-product options)
                :when (not (some (fn [[x y]] (= x y)) (map vector (rest new-state) (butlast new-state))))]
          (swap! new-states (partial merge-with +) {new-state combinations})))
      (reset! states @new-states))
    (let [final-state-A (for [x (range high)] (* 2 (mod x 2)))
          final-state-B (for [x (range high)] (* 2 (mod (inc x) 2)))]
      (+ (@states final-state-A) (@states final-state-B)))))

(comment
  (time (euler-215-phyn 32 10))
  )
