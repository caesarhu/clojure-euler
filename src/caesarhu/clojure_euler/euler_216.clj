(ns caesarhu.clojure-euler.euler-216
  (:require [caesarhu.math.primes :as p]
            [clojure.math.numeric-tower :refer [sqrt]]
            [caesarhu.math.quadratic-residue :refer [tonelli]]))

(defn p->np
  [^long p]
  (let [a (tonelli (quot (inc p) 2) p)]
    (->> (min a (- p a)) long)))

(defn sieve-numbers
  [^long limit, ^long prime]
  (let [np (p->np prime)]
    (->> (iterate #(+ prime %) prime)
         (mapcat #(list (- % np) (+ % np)))
         (take-while #(<= % limit)))))

(defn euler-216
  [^long limit]
  (let [prime-array (boolean-array (inc limit) true)
        prime-bound (->> (* (sqrt 2) limit) inc long)
        legal-set #{1 7}
        primes (->> (p/primes prime-bound)
                    (filter #(legal-set (mod % 8))))]
    (doseq [p primes
            m (sieve-numbers limit p)]
      (aset prime-array m false))
    (reduce (fn [sum i]
              (if (aget prime-array i) (inc sum) sum))
            0
            (range 2 (inc limit)))))

(comment
  (time (euler-216 50000000))
  )
