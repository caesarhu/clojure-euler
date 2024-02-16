(ns caesarhu.clojure-euler.euler-186
  (:require [clojure.core.memoize :as mem]))

(def limit 1000000)

(def lfg
  (mem/fifo
   (fn [k]
     (if (<= k 55)
       (mod (+ 100003 (- (* 200003 k)) (* 300007 k k k)) limit)
       (mod (+ (lfg (- k 24)) (lfg (- k 55))) limit)))
   :fifo/threshold 56)) ; :fifo/threshold 要設置56=55+1

(defn call
  [n]
  (let [i (* 2 n)
        caller (lfg (dec i))
        called (lfg i)]
    (when (not= caller called) [caller called])))

(def pm 524287)
(def target (int (* limit 0.99)))
(def parent (atom nil))
(def children (atom nil))

(defn init-atom
  []
  (reset! parent (vec (range limit)))
  (reset! children (vec (repeat limit 1))))

(defn head
  [x]
  (if (= (@parent x) x) x
      (recur (@parent x))))

(defn union
  [x y]
  (let [a (head x)
        b (head y)]
    (when (not= a b)
      (if (< (@children a) (@children b))
        (recur b a)
        (do
          (swap! parent assoc b a)
          (swap! children assoc a (+ (@children a) (@children b)))
          nil)))))

(defn solve
  []
  (init-atom)
  (loop [i 1
         succeed 0]
    (if (>= (@children (head pm)) target)
      succeed
      (if-let [[caller called] (call i)]
        (do
          (union caller called)
          (recur (inc i) (inc succeed)))
        (recur (inc i) succeed)))))

(comment
  (time (solve))
  )
