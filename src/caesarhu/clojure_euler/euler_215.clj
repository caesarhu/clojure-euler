(ns caesarhu.clojure-euler.euler-215
  (:require [clojure.math.combinatorics :as c]
            [com.rpl.specter :as s]
            [clojure.set :refer [intersection]]
            [medley.core :refer [insert-nth]]))

(def default-bricks [2 3])

(defn width-bricks
  [width]
  (let [[x y] default-bricks]
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
  ([legal? row]
   (let [width (apply + row)
         gaps (set (reductions + (butlast row)))]
     (loop [next-rows (list [])
            result (list)]
       (if (empty? next-rows)
         (if legal?
           (filter legal? result)
           result)
         (let [new-next-rows (->> (mapcat (fn [row] (map #(conj row %) default-bricks)) next-rows)
                                  (remove #(> (apply + %) width))
                                  (remove #(gaps (apply + %))))
               matched (filter #(= width (apply + %)) new-next-rows)]
           (recur new-next-rows (concat result matched)))))))
  ([row]
   (gen-next-rows nil row)))

(defn euler-215-old
  [width high]
  (let [bricks-v (width-bricks width)
        origin-rows (mapcat permutations-bricks bricks-v)
        match-bricks? (apply gen-bricks-fn bricks-v)
        origin-map (->> origin-rows
                        (map #(vector % (gen-next-rows match-bricks? %)))
                        (remove #(empty? (last %)))
                        (into {}))]
    (if (= 1 high)
      (count origin-rows)
      (loop [level 2
             level-map (s/transform [s/MAP-VALS] count origin-map)]
        (if (>= level high)
          (->> (vals level-map) (apply +'))
          (recur (inc level)
                 (->> (for [[row lower-rows] origin-map
                            :let [n (level-map row)]
                            lower-row lower-rows]
                        {lower-row n})
                      (apply merge-with +'))))))))

(comment
  (time (euler-215-old 32 10))
  )

(defn gen-top-rows
  [width]
  (->> (for [i (range (inc (quot width 3)))
             :let [new-width (- width (* i 3))]
             :when (zero? (mod new-width 2))]
         (for [pos (c/combinations (range (+ i (quot new-width 2))) i)
               :let [bricks (atom (repeat (quot new-width 2) 2))]]
           (do (doseq [x pos]
                 (swap! bricks #(insert-nth x 3 %)))
               (->> (reductions + (butlast @bricks)) vec))))
       (apply concat)))

(defn gen-sub-rows
  [width row]
  (loop [sub-rows (map vector default-bricks)
         result (list)]
    (if (empty? sub-rows)
      result
      (let [next-rows (->> (mapcat (fn [row]
                                     (map #(conj row (+ (last row) %)) default-bricks)) sub-rows)
                           (filter #(empty? (intersection (set %) (set row))))
                           (remove #(> (last %) width)))
            matched (filter #(= width (last %)) next-rows)]
        (recur next-rows (concat result (map #(vec (butlast %)) matched)))))))

(defn gen-rows-map
  [width]
  (->> (gen-top-rows width)
       (map #(hash-map % (gen-sub-rows width %)))
       (apply merge)))

(defn euler-215
  [width high]
  (let [rows-map (gen-rows-map width)]
    (cond
      (<= high 0) 0
      (= high 1) (count rows-map)
      :else (loop [level 2
                   level-map (s/transform [s/MAP-VALS] count rows-map)]
              (if (>= level high)
                (->> (vals level-map) (apply +'))
                (recur (inc level)
                       (->> (for [[row lower-rows] rows-map
                                  :let [n (level-map row)]
                                  lower-row lower-rows]
                              {lower-row n})
                            (apply merge-with +'))))))))

(comment
  (time (euler-215 32 10))
  )
