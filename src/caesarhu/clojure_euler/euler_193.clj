(ns caesarhu.clojure-euler.euler-193
  (:require [clojure.math.numeric-tower :refer [expt exact-integer-sqrt sqrt]]
            [caesarhu.math.primes :as p]))

; base on https://smsxgz.github.io/post/pe/counting_square_free_numbers/

(defn isqrt
  [n]
  (-> (exact-integer-sqrt n) first))

(defn mobius
  [limit]
  (loop [i 2
         prime (vec (repeat (inc limit) 1))
         m-vec (vec (repeat (inc limit) 1))]
    (cond
      (> i limit) m-vec
      (zero? (prime i)) (recur (inc i) prime m-vec)
      :else (let [[new-prime new-m-vec] (reduce (fn [[p new] j]
                                                  [(assoc p (* i j) 0) (update new (* i j) #(* % -1))])
                                                [prime (assoc m-vec i -1)]
                                                (range 2 (inc (quot limit i))))]
              (recur (inc i) new-prime (reduce (fn [new j]
                                                 (assoc new (* j i i) 0))
                                               new-m-vec
                                               (range 1 (inc (quot limit (* i i))))))))))

(defn square-free
  [limit]
  (let [sq (isqrt limit)
        mobius-vec (mobius sq)]
    (->> (for [i (range 1 (inc sq))]
           (* (mobius-vec i) (quot limit (* i i))))
         (apply +))))

(defn mertens
  [mobius-vec]
  (last (reduce (fn [[sum m-vec] mobius]
                  (let [new-sum (+ sum mobius)]
                    [new-sum (conj m-vec new-sum)]))
                [0 [0]]
                (rest mobius-vec))))

(defn efficient-square-free
  [N imax]
  (let [D (long (sqrt (/ N imax)))
        mobius-vec (mobius D)
        s1 (->> (for [i (range 1 (inc D))]
                  (* (mobius-vec i) (quot N (* i i))))
                (apply +))
        M-list (mertens mobius-vec)
        s2 (loop [i (dec imax)
                  mxi-list []
                  mxi-sum 0]
             (if (zero? i)
               (- mxi-sum (* (dec imax) (last M-list)))
               (let [xi (isqrt (quot N i))
                     spd (isqrt xi)
                     mxi-pre (reduce (fn [mxi j]
                                       (- mxi (* (- (quot xi j) (quot xi (inc j))) (M-list j))))
                                     1
                                     (range 1 (inc (quot xi (inc spd)))))
                     mxi (reduce (fn [mxi j]
                                   (if (<= (quot xi j) D)
                                     (- mxi (M-list (quot xi j)))
                                     (- mxi (mxi-list (- imax (* j j i) 1)))))
                                 mxi-pre
                                 (range 2 (inc spd)))]
                 (recur (dec i) (conj mxi-list mxi) (+ mxi-sum mxi)))))]
    (+ s1 s2)))

(defn euler-193
  [limit]
  (efficient-square-free limit (long (expt limit 1/5))))

(comment
  (time (euler-193 (expt 2 50)))
  )
