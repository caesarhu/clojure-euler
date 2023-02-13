(ns caesarhu.clojure-euler.euler-146
  (:require [caesarhu.math.primes :as p]
            [caesarhu.math.math-tools :refer [square? power-mod]]
            [caesarhu.math.quadratic-residue :refer [legendre cipolla]]
            [caesarhu.math.chinese-remainder :refer [crt crt-seq]]
            [clojure.math.combinatorics :refer [cartesian-product]]))

(def add-primes [1 3 7 9 13 27])
(def not-primes [19 21])

(defn fermat-test?
  [n p]
  (= 1 (power-mod n (dec p) p)))

(defn valid?
  [n]
  (let [n2 (*' n n)
        s1 (map #(+ n2 %) add-primes)
        s2 (map #(+ n2 %) not-primes)]
    (and (fermat-test? 3 (first s1))
         (every? #(fermat-test? n %) (rest s1))
         (every? #(not (fermat-test? n %)) s2))))

(defn quadratic-residues
  [p]
  (if (= p 2)
    [0 1]
    (->> (range 1 p)
         (filter #(= 1 (legendre % p)))
         (cons 0))))

(defn filter-residues
  [p]
  (for [r (quadratic-residues p)
        :when (every? #(pos? (mod (+ % r) p)) add-primes)]
    r))

(defn prime-residues
  [limit]
  (let [pr (->> (p/primes limit)
                (map #(vector % (filter-residues %)))
                (map (fn [[p rv]]
                       [p (->> (map #(if (zero? %) % (cipolla % p)) rv)
                               flatten
                               (map long))])))
        [pr-head pr-tail] (split-at 6 pr)
         primes (map first pr-head)]
    [[(apply *' primes)
      (->> (map last pr-head)
           (apply cartesian-product)
           (map #(crt primes %))
           sort)]
     (->> pr-tail
          (map #(vector (first %) (into #{} (last %))))
          (into {}))]))

(defn euler-146
  [limit]
  (let [[[step rv] m] (prime-residues 80)]
    (->> (range)
         (map #(*' % step))
         (mapcat #(map (partial +' %) rv))
         (take-while #(< % limit))
         (filter (fn [n] (every? (fn [[p s]] (s (mod n p))) m)))
         (filter valid?)
         ((fn [v] [(apply +' v) v])))))

(comment
  (time (euler-146 (* 150 1000000)))
  )