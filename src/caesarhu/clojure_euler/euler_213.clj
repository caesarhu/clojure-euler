(ns caesarhu.clojure-euler.euler-213
  (:require [clojure.math.numeric-tower :refer [round]]
            [clojure.core.matrix :as m]))

(defonce implementation (m/set-current-implementation :vectorz))

(def direction [[1 0] [-1 0] [0 1] [0 -1]])

(defn create-next-step
  [size]
  (fn [x y]
    (let [size-set (set (range size))
          legal? #(size-set %)]
      (->> (map #(map + %1 %2) (repeat [x y]) direction)
           (filter #(every? legal? %))
           (map vec)))))

(defn init-box
  [size n]
  (->> (repeat size (vec (repeat size n))) vec))

(defn next-state
  [box]
  (let [size (count box)
        next-step (create-next-step size)
        next-box (atom (init-box size 0))]
    (doseq [i (range size)
            j (range size)
            [x y] (next-step i j)
            :let [length (count (next-step x y))
                  probability (/ (get-in box [x y]) length)]]
      (swap! next-box update-in [i j] + probability))
    @next-box))

(defn solve
  [none size steps [x y]]
  (let [current (loop [step 0
                       current (assoc-in (init-box size 0) [x y] 1)]
                  (if (>= step steps)
                    current
                    (recur (inc step) (next-state current))))]
    (doseq [i (range size)
            j (range size)]
      (swap! none update-in [i j] * (- 1 (get-in current [i j])))
      (when (not= i (- size 1 i))
        (swap! none update-in [i j] * (- 1 (get-in current [(- size 1 i) j]))))
      (when (not= j (- size 1 j))
        (swap! none update-in [i j] * (- 1 (get-in current [i (- size 1 j)]))))
      (when (and (not= i (- size 1 i))
                 (not= j (- size 1 j)))
        (swap! none update-in [i j] * (- 1 (get-in current [(- size 1 i) (- size 1 j)])))))
    (println [x y] "finished!")))

(defn brute-force
  [size steps]
  (let [none (atom (init-box size 1))]
    (doseq [i (range (quot (inc size) 2))
            j (range (quot (inc size) 2))]
      (solve none size steps [i j]))
    (->> (for [i (range size)
               j (range size)]
           (get-in @none [i j]))
         (apply +)
         (* 1000000)
         round
         (#(/ % 1000000))
         double)))

(comment
  (time (brute-force 30 50))
  ; 大約要執行超過24小時
  )

(defn trans-matrix
  [n]
  (let [sqr (* n n)
        trans-mat (atom (vec (repeat sqr (vec (repeat sqr 0)))))]
    (doseq [i (range sqr)]
      (let [col (mod i n)
            row (quot i n)
            n-poss (+ (if (not= col 0) 1 0)
                      (if (not= col (dec n)) 1 0)
                      (if (> row 0) 1 0)
                      (if (< row (dec n)) 1 0))]
        (when (not= col 0)
          (swap! trans-mat assoc-in [i (dec i)] (/ n-poss)))
        (when (not= col (dec n))
          (swap! trans-mat assoc-in [i (inc i)] (/ n-poss)))
        (when (> row 0)
          (swap! trans-mat assoc-in [i (- i n)] (/ n-poss)))
        (when (< row (dec n))
          (swap! trans-mat assoc-in [i (+ i n)] (/ n-poss)))))
    @trans-mat))

(defn mpower
  [a b]
  (loop [r a n a e (dec b)]
    (cond
      (zero? e) r
      (= 1 (mod e 2)) (recur (m/mmul r n) n (dec e))
      :else (recur r (m/mmul n n) (quot e 2)))))

(defn euler-213
  [size steps]
  (let [nsq (* size size)
        trans-mat (trans-matrix size)
        total-trans-mat (mpower trans-mat steps)]
    (->> (m/sub 1 total-trans-mat)
         (apply map *)
         (apply +))))

(comment
  (time (euler-213 30 50))
  ; "Elapsed time: 255272.421042 msecs"
  ; 還是很慢，但clojure沒有像numpy這樣優秀的函數庫，只能優化至此。
  )
