(ns caesarhu.project-euler.euler-035
  (:require [caesarhu.math.math-tools :as tools]
            [caesarhu.math.primes :as p]))

(defn rotate
  [n]
  (let [ds (tools/digits n)
        r (fn [ds] (concat (rest ds) [(first ds)]))]
    (->> (take (count ds) (iterate r ds))
         (map tools/digits->number))))

(defn euler-035
  [limit]
  (let [primes-set (set (p/primes limit))
        circular-prime? (fn [p]
                          (and (every? odd? (tools/digits p))
                               (every? #(primes-set %) (rotate p))))]
    (-> (filter circular-prime? primes-set)
        (conj 2)
        count)))

(comment
  (time (euler-035 1000000))
  )