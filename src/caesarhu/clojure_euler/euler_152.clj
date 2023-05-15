(ns caesarhu.clojure-euler.euler-152
  (:require [caesarhu.math.math-tools :as mt]
            [clojure.math.numeric-tower :refer [expt]]
            [clojure.set :as set]
            [caesarhu.math.primes :as p]
            [clojure.math.combinatorics :as combo]))

(defn square [n] (* n n))

(defn square-sum
  [s]
  (if (empty? s)
    [0 0 0]
    (let [lcm (apply mt/lcm* s)
          n (->> (map #(quot lcm %) s)
                 (map square)
                 (apply +))]
      [n (square lcm) (/ n (square lcm))])))

(defn gen-seq
  [n illegal]
  (->> (range 1 (inc n))
       (filter #(mt/coprime? illegal %))
       (combo/subsets)))

(defn prime-sets
  ([limit p]
   (prime-sets limit p 1))
  ([limit p illegal]
   (let [pp (->> (or ({2 3 3 2} p) 1) (expt p))
         n (quot limit pp)]
     (->> (drop 2 (gen-seq n illegal))
          (map #(map (partial * pp) %))
          (filter #(zero? (mod (first (square-sum %)) (square p))))))))

(defn euler-152
  [limit]
  (let [[valid invalid] (vals (group-by #(some? (first (prime-sets limit %)))
                                        (p/primes (inc (quot limit 5)))))
        target (->> (p/divisors (* 2 2 3)) rest combo/subsets rest
                    (map #(hash-map (last (square-sum %)) %))
                    (apply merge))
        match-target (fn [s]
                       (when-let [ans (->> (last (square-sum s)) (- 1/2) target)]
                         (concat ans s)))
        illegal (apply * 1 invalid)]
    (loop [[prime & more] (reverse valid)
           legal 1
           result [[]]]
      (if (nil? prime)
        (->> (map match-target result)
             (remove nil?)
             (map set)
             (into #{})
             count)
        (recur more
               (if (>= prime 5)
                 (* legal prime)
                 legal)
               (->> (combo/cartesian-product (cons [] (prime-sets limit prime illegal)) result)
                    (map #(distinct (apply concat %)))
                    (filter #(or (empty? %)
                                 (mt/coprime? legal (-> (square-sum %) last denominator))))))))))

(comment
  (->> (prime-sets 80 7))
  (time (euler-152 100))
  )
