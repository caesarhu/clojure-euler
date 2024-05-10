(ns caesarhu.clojure-euler.euler-051
  (:require [caesarhu.math.math-tools :as tools]
            [caesarhu.math.primes :as p]))

(def prime-set (set (p/primes 1000000)))

(defn count-primes
  [s]
  (->> (filter prime-set s)
       count))

(defn change-digit
  [v d1 d2]
  (reduce (fn [acc d]
            (if (= d d1)
              (conj acc d2)
              (conj acc d)))
          [] v))

(defn transfrom
  [n]
  (let [digits (tools/digits n)
        target-digits (fn [d]
                        (if (= d (first digits))
                          (range 1 10)
                          (range 10)))]
    (for [d1 (distinct digits)]
      (for [d2 (target-digits d1)]
        (tools/digits->number (change-digit digits d1 d2))))))

(defn euler-051
  [n]
  (some (fn [p]
          (and (some #(>= (count-primes %) n) (transfrom p)) p))
        (drop-while #(< % 100000) (p/primes))))

(comment
  (time (euler-051 8))
  )
