(ns caesarhu.project-euler.euler-043
  (:require [caesarhu.math.math-tools :refer [digits->number]]
            [caesarhu.math.primes :refer [primes]]))

(defn add-digit-divisible
  [v p]
  (let [v-set (set v)]
    (for [i (range 10)
          :when (not (v-set i))
          :let [vi (cons i v)
                t (take 3 vi)]
          :when (zero? (mod (digits->number t) p))]
      vi)))

(defn filter-rules
  [primes]
  (let [init (map vector (range 10))]
    (reduce (fn [acc p]
              (mapcat #(add-digit-divisible % p) acc))
            init primes)))

(defn euler-043
  []
  (let [primes [1 17 13 11 7 5 3 2 1]]
    (->> (filter-rules primes)
         (map digits->number)
         (apply +))))

(comment
  (time (euler-043))
  )