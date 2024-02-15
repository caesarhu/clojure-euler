(ns caesarhu.clojure-euler.euler-183
  (:require [clojure.math :as m]))

(defn quot-recur
  [n p]
  (loop [n n]
    (if (not (zero? (mod n p)))
      n
      (recur (quot n p)))))

(defn terminating?
  [r]
  (if (== (long r) r)
    true
    (let [d (denominator (rationalize r))]
      (-> (quot-recur d 2)
          (quot-recur 5)
          (= 1)))))

(defn calc-log
  [n k]
  (* k (Math/log (/ n k))))

(defn guess-number
  [n]
  (let [x (long (/ n m/E))
        k (apply max-key #(calc-log n %) [x (inc x)])]
    (if (terminating? (/ n k))
      [(- n) k]
      [n k])))

(defn euler-183
  [limit]
  (->> (range 5 (inc limit))
       (map guess-number)
       (map first)
       (apply +)))

(comment
  (time (euler-183 10000))
  )
