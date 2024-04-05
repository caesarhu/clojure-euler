(ns caesarhu.clojure-euler.euler-216
  (:require [caesarhu.math.primes :as p]
            [caesarhu.math.math-tools :refer [power-mod]]
            [caesarhu.math.quadratic-residue :refer [legendre]]))

(defn square
  [^long n]
  (* n n))

(defn n->p
  [^long n]
  (dec (* 2 n n)))

(defn prime-test
  [^long n]
  (let [p (n->p n)]
    (and
      (= 1 (legendre (square n) p) (legendre 2 p))
      (p/probable-prime? p))))

(defn euler-216-slow
  [limit]
  (->> (iterate inc 2)
       (take-while #(<= % limit))
       (filter prime-test)
       count))

(defn euler-216
  [limit]
  (let [tn-vec (atom (vec (repeat (inc limit) 0)))
        primes (atom (vec (repeat (inc limit) 1)))
        elimination (fn [place p]
                      (loop [idx place]
                        (when (<= idx limit)
                          (let [tn (if (zero? (mod (@tn-vec idx) p))
                                     (do
                                       (swap! primes assoc idx 0)
                                       (loop [tn (@tn-vec idx)]
                                         (if (zero? (mod tn p))
                                           (recur (quot tn p))
                                           tn)))
                                     (@tn-vec idx))]
                            (swap! tn-vec assoc idx tn)
                            (recur (+ idx p))))))]
    (swap! primes assoc 0 0)
    (swap! primes assoc 1 0)
    (doseq [i (range 1 (inc limit))]
      (swap! tn-vec assoc i (n->p i)))
    (doseq [x (range 1 (inc (quot limit 2)))
            :let [p (@tn-vec x)]]
      (when (> p 1)
        (elimination (+ p x) p)
        (elimination (- p x) p)))
    (apply + @primes)))

(comment
  (time (euler-216 50000000))
  (time (euler-216-slow 50000000))
  )
