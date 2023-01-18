(ns caesarhu.clojure-euler.euler-133
  (:require [caesarhu.math.primes :as p]
            [caesarhu.math.math-tools :refer [power-mod]]
            [clojure.math.numeric-tower :refer [expt]]))

(defn power-of-prime
  [limit p]
  (->> (for [px (map #(expt p %) (iterate inc 1))
             :while (<= px limit)]
         (for [t (iterate inc 1)
               :let [tpx (* t px)]
               :while (<= tpx limit)]
           {tpx {p 1}}))
       flatten
       (apply merge-with (fn [m1 m2] (merge-with + m1 m2)))))

(defn factors-range
  [limit]
  (->> (for [p (take-while #(< % limit) (p/primes))]
         (power-of-prime limit p))
       (apply merge-with (fn [m1 m2] (merge-with + m1 m2)) (sorted-map))))

(defn divisors
  [factor-map n]
  (let [m (factor-map n)
        product-coll (fn [c1 c2]
                       (cond
                         (empty? c1) c2
                         (empty? c2) c1
                         :else (for [x c1 y c2]
                                 (*' x y))))]
    (->> (map (fn [[n power]] (for [i (range (inc power))] (expt n i))) m)
         (reduce product-coll)
         (into (sorted-set)))))

(defn repunit-length
  [factor-map p]
  (let [factor? (fn [p n] (= 1 (power-mod 10 n (* 9 p))))]
    (some #(and (factor? p %) %) (divisors factor-map (dec p)))))

(defn euler-133
  [limit]
  (let [factor-map (factors-range limit)
        power-10? (fn [p] (when (> p 5)
                            (-> (repunit-length factor-map p)
                                factor-map
                                (dissoc 2 5)
                                empty?)))]
    (->> (p/primes limit)
         (remove power-10?)
         (apply +))))

;----------------------------------------------------------------------

(defn factor?
  [n p]
  (let [phi (fn [p] (dec p))
        mn (mod n (phi p))]
    (and (> p 5) (= 1 (power-mod 10 mn (* 9 p))))))

(defn factor-10n?
  [limit p]
  (let [power (->> (map #(vector % (expt 2 %)) (iterate inc 1)) 
                   (take-while #(< (last %) limit))
                   last
                   first)]
    (factor? (expt 10 power) p)))

(defn euler-133-fast
  [limit]
  (->> (p/primes limit)
       (remove (partial factor-10n? limit))
       (apply +)))

(comment
  (time (euler-133-fast 100000))
  )