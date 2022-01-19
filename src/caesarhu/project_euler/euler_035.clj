(ns caesarhu.project-euler.euler-035
  (:require [caesarhu.math.math-tools :as tools]
            [caesarhu.math.primes :as p]))

(defn rotate
  [n]
  (let [ds (tools/digits n)
        r (fn [ds] (concat (rest ds) [(first ds)]))]
    (->> (take (count ds) (iterate r ds))
         (map tools/digits->number))))

(defn circular-prime?
  [ps n]
  (->> (rotate n)
       (every? #(ps %))))

(defn euler-035
  [limit]
  (let [ps (set (take-while #(< % limit) p/primes))]
    (count (filter #(circular-prime? ps %) ps))))

(comment
  (time (euler-035 1000000))
  )