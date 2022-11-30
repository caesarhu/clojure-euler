(ns caesarhu.project-euler.euler-060
  (:require [caesarhu.math.primes :as p]
            [caesarhu.math.math-tools :as tools]))

(defn concatenated?
  [p1 p2]
  (let [d1 (tools/digits p1)
        d2 (tools/digits p2)]
    (->> [(concat d1 d2) (concat d2 d1)]
         (map tools/digits->number)
         (every? p/is-prime?))))

(defn next-map
  [m]
  (->> (for [[k v] m
             p v
             :let [ps (filter #(concatenated? p %) v)]]
         {(conj k p) ps})
       (apply merge)))

(defn euler-060
  [limit n]
  (loop [m {#{} (p/primes limit)}
         n n]
    (if (zero? n)
      (->> (keys m) (map #(apply + %)) (apply min))
      (recur (next-map m) (dec n)))))

(comment
  (time (euler-060 10000 5))
  )