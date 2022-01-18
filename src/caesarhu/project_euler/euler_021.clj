(ns caesarhu.project-euler.euler-021
  (:require [caesarhu.math.primes :as p]))

(defn proper-divisors-sum
  [n]
  (apply + (butlast (p/divisors n))))

(defn euler-021
  [limit]
  (let [sum-map (->> (range 2 limit)
                     (map #(vector % (proper-divisors-sum %)))
                     (into {}))]
    (loop [i 2
           m sum-map
           amicable-numbers []]
      (if (= limit i) [(->> (flatten amicable-numbers) (apply +))
                       amicable-numbers]
          (let [sum (m i)]
            (if (and sum (not= i sum) (= i (m sum)))
              (recur (inc i) (dissoc m i sum) (conj amicable-numbers [i sum]))
              (recur (inc i) m amicable-numbers)))))))

(comment
  (time (euler-021 10000))
  )