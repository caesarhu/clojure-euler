(ns caesarhu.clojure-euler.euler-214
  (:require [caesarhu.math.primes :as p]
            [clojure.math.numeric-tower :refer [expt]]))

(defn euler-214
  [limit target]
  (let [primes (p/primes limit)
        prime-set (set primes)
        totient (fn [n]
                  (cond
                    (= 1 n) 1
                    (prime-set n) (dec n)
                    :else (loop [ps (take-while #(<= (* % %) n) primes)
                                 n n
                                 phi n]
                            (if-let [p (first ps)]
                              (if (zero? (mod n p))
                                (recur (rest ps)
                                       (loop [n n]
                                         (if (pos-int? (mod n p)) n
                                             (recur (quot n p))))
                                       (- phi (quot phi p)))
                                (recur (rest ps) n phi))
                              (if (> n 1)
                                (- phi (quot phi n))
                                phi)))))
        -chain (fn [length n]
                 (cond
                   (< n 4) (+ length n)
                   (> length target) length
                   :else (recur (inc length) (totient n))))
        chain (fn [n] (-chain 1 (totient n)))
        low-bound (expt 2 23)]
    (transduce (filter #(= target (chain %)))
               + (drop-while #(< % low-bound) primes))))


(comment
  (time (euler-214 40000000 25))
  ; "Elapsed time: 249535.795167 msecs"
  ; 1677366278943
  )
