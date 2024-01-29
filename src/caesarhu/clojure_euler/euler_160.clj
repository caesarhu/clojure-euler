(ns caesarhu.clojure-euler.euler-160
  (:require [caesarhu.math.math-tools :refer [power-mod divmod]]
            [clojure.math.numeric-tower :refer [expt]]
            [caesarhu.math.chinese-remainder :as crt]))

(defn mod-inv
  [a b]
  (let [b (if (neg? b) (- b) b)
        a (if (neg? a) (- b (mod (- a) b)) a)
        egcd (crt/extended-gcd a b)]
    (when (= (first egcd) 1)
      (mod (second egcd) b))))

(defn info
  [n p e]
  (let [m (expt p e)
        product-not-5n (fn [n]
                         (let [[q r] (divmod n m)
                               rem (reduce #(mod (* %1 %2) m)
                                           1
                                           (->> (range 1 (inc r))
                                                (remove #(zero? (mod % p)))))]
                           (if (odd? q)
                             (mod (- rem) m)
                             rem)))]
    (loop [n n
           ee 0
           rem 1]
      (if (zero? n)
        [ee rem]
        (recur (quot n 5) (+ ee (quot n 5)) (mod (* rem (product-not-5n n)) m))))))

(defn euler-160
  [n d]
  (let [[e !5] (info n 5 d)
        r2 (power-mod 2 e (expt 5 d))
        r2-inv (mod-inv r2 (expt 5 d))
        r5 (mod (* !5 r2-inv) (expt 5 d))]
    (int (crt/crt [(expt 2 d) (expt 5 d)] [0 r5]))))

(comment
  (euler-160 10 5)
  (time (euler-160 (expt 10 10000) 5))
  )
