(ns caesarhu.clojure-euler.euler-096
  (:require [clojure.string :as str]
            [caesarhu.math.math-tools :refer [digits->number]]
            [caesarhu.kuafu.sat :as sat]))

(def sudoku-file "resources/p096_sudoku.txt")

(defn get-sudoku-puzzles
  [fname]
  (->> (str/split-lines (slurp fname))
       (partition 10)
       (map rest)
       (map (fn [s]
              (for [ss s]
                (vec (map #(- (int %) 48) ss)))))
       (map vec)))

(defn sodoku-lines
  [sodoku]
  (let [v (mapcat #(partition 3 %) sodoku)
        boxs (for [i (range 3)
                   j (range 3)]
               (->> (take 3 (iterate #(+ 3 %) j))
                    (map #(+ (* i 9) %))
                    (mapcat #(nth v %))))]
    (concat sodoku boxs
            (map (fn [i]
                   (map #(get-in sodoku [% i]) (range 9))) (range 9)))))

(defn sodoku-solver
  [sodoku]
  (let [model (sat/cp-model)
        solver (sat/cp-solver)
        domain (sat/domain 1 9)
        var-grid (vec (for [i (range 9)]
                        (vec (for [j (range 9)]
                               (sat/int-var model domain)))))]
    (doseq [vars (sodoku-lines var-grid)]
      (sat/add-all-different model vars))
    (doseq [i (range 9)
            j (range 9)]
      (let [n (get-in sodoku [i j])]
        (when (not (zero? n))
          (sat/add-equality model (get-in var-grid [i j]) n))))
    (sat/set-all-solutions solver true)
    (reset! sat/*solutions* (list))
    (sat/solve solver model (sat/callback (flatten var-grid)))
    (first @sat/*solutions*)))

(defn euler-096
  []
  (->> (get-sudoku-puzzles sudoku-file)
       (map sodoku-solver)
       (map #(take 3 %))
       (map digits->number)
       (apply +)))

(comment
  (time (euler-096))
  )