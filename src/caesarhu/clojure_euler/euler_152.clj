(ns caesarhu.clojure-euler.euler-152
  (:require [caesarhu.math.math-tools :as mt]
            [clojure.set :as set]
            [caesarhu.math.primes :as p]
            [clojure.math.combinatorics :as combo]))

(defn square [n] (* n n))

(defn square-sum
  [s]
  (let [lcm (apply mt/lcm* s)
        n (->> (map #(quot lcm %) s)
               (map square)
               (apply +))]
    [n (square lcm)]))

(defn inverse-sq
  [s]
  (apply / (square-sum s)))

(defn gen-seq-map
  [limit]
  (let [n (quot limit 5)
        primes (p/primes 5 (inc n))
        raw-map (->> (drop 2 (combo/subsets (range 1 (inc n))))
                     (map #(vector (square-sum %) %))
                     (mapcat (fn [[[n d] s]]
                               (when-let [valid (->> (filter (fn [p] (every? #(<= (* p %) limit) s)) primes)
                                                     (filter #(zero? (mod n (square %))))
                                                     not-empty)]
                                 (map #(hash-map % [(map (partial * %) s)]) valid))))
                     (apply merge-with concat))
        invalid (apply * 1 (set/difference (set primes) (set (keys raw-map))))]
    (reduce (fn [m k]
              (update m k (fn [ss] (filter (fn [s] (every? #(mt/coprime? invalid %) s)) ss))))
            raw-map (keys raw-map))))

(defn euler-152
  [limit]
  (let [seq-map (gen-seq-map limit)
        target (->> (p/divisors (* 2 2 2 3 3)) rest (filter #(<= % limit))
                    combo/subsets rest
                    (map #(hash-map (inverse-sq %) [%]))
                    (apply merge-with concat))
        match-target (fn [s]
                       (when-let [ans (->> (inverse-sq s) (- 1/2) target)]
                         (map #(concat % s) ans)))]
    (loop [[prime & more] (-> (keys seq-map) sort reverse)
           legal 1
           result [[]]]
      (if (nil? prime)
        (->> (mapcat match-target (rest result))
             (map set)
             (into #{})
             count)
        (let [new-legal (* prime legal)]
          (recur more
                 new-legal
                 (sequence (comp
                            (map #(distinct (apply concat %)))
                            (filter #(or (empty? %)
                                         (mt/coprime? new-legal (denominator (inverse-sq %))))))
                           (combo/cartesian-product (cons [] (seq-map prime)) result))))))))

(comment
  (time (euler-152 80))
  )