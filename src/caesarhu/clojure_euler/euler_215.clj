(ns caesarhu.clojure-euler.euler-215
  (:require [clojure.math.combinatorics :as c]
            [clojure.set :as s]
            [clojure.math.numeric-tower :as math]))

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
        gaps (set (reductions + (butlast row)))
        result (atom (list))
        gen (fn [rows]
              (for [next-row rows
                    n bricks
                    :let [new-next-row (conj next-row n)
                          sum (apply + new-next-row)]
                    :when (and (<= sum width)
                               (not (gaps sum)))]
                (do
                  (when (= width sum)
                    (swap! result conj new-next-row))
                  new-next-row)))]
    (loop [next-rows (list [])]
      (if (empty? next-rows)
        @result
        (recur (gen next-rows))))))

(defn rows-product
  [legal? rows]
  (->> (mapcat #(gen-next-rows %) rows) (filter legal?)))

(defn brute-force
  [width high]
  (let [bricks-v (row-bricks width)
        crack-free? (apply gen-bricks-fn bricks-v)]
    (->> (reduce (fn [rows _]
                   (rows-product crack-free? rows))
                 (mapcat permutations-bricks bricks-v)
                 (range (dec high)))
         count)))

(comment
  (time (brute-force 9 3))
  )

; https://projecteuler.net/thread=215;page=8 phyn's solution
(defn euler-215
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
  (time (euler-215 32 10))
  )
