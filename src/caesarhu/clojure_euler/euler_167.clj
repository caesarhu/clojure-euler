(ns caesarhu.clojure-euler.euler-167
  (:require [clojure.math.numeric-tower :refer [expt]]))

(defn calc-period
  [v]
  (let [mask (dec (expt 2 (inc v)))
        is-contained? (fn [indicator]
                        (= (+ (bit-shift-right indicator v) (bit-and indicator 1)) 1))
        next-indicator (fn [indicator]
                         (if (is-contained? indicator)
                           (bit-and (inc (bit-shift-left indicator 1)) mask)
                           (bit-and (bit-shift-left indicator 1) mask)))
        double-next-indicator (fn [indicator]
                                (next-indicator (next-indicator indicator)))]
    (loop [tortoise 1
           hare tortoise
           pos 1
           counter 1
           m {0 v}]
      (let [next-tortoise (next-indicator tortoise)
            next-hare (double-next-indicator hare)
            next-counter (if (is-contained? tortoise) (inc counter) counter)]
        (if (= next-tortoise next-hare)
          [(* pos 2) counter m]
          (recur next-tortoise
                 next-hare
                 (inc pos)
                 next-counter
                 (if (is-contained? tortoise)
                   (merge m {counter (+ v (* pos 2))})
                   m)))))))

(defn ulam
  [n k]
  (let [b (inc (* n 2))
        modified-k (- k 3)
        [diff period m] (calc-period b)
        q (quot modified-k period)
        r (mod modified-k period)]
    (if (zero? q)
      (->> (concat [2 (* 2 (inc b))] (vals m))
           sort
           (#(nth % (dec k))))
      (+ (* q diff) (m r)))))

(defn euler-167
  []
  (->> (range 2 11)
       (map #(ulam % (expt 10 11)))
       (apply +)))

(comment
  (time (euler-167))
  )
