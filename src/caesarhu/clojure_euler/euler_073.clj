(ns caesarhu.clojure-euler.euler-073
  (:require [clojure.math.numeric-tower :as math]
            [caesarhu.math.primes :as p]))

(defn count-ratio
  [bound n]
  (let [[low hi] bound
        low-bound (let [ln (* n low)
                        round-ln (math/ceil ln)] 
                    (long (if (= round-ln ln) (inc round-ln) round-ln)))
        hi-bound (let [hn (* n hi)
                       round-hn (math/floor hn)]
                   (long (if (= round-hn hn) (dec round-hn) round-hn)))]
    (transduce (comp
                (map #(math/gcd % n))
                (filter #(= % 1)))
               + (range low-bound (inc hi-bound)))))

(defn euler-073
  [limit]
  (transduce (map #(count-ratio [1/3 1/2] %))
             + (range 2 (inc limit))))

(comment
  (time (euler-073 12000))
  )