(ns caesarhu.clojure-euler.euler-152-2
  (:require [caesarhu.math.primes :as p]
            [clojure.math.numeric-tower :refer [expt]]
            [caesarhu.math.math-tools :refer [lcm* gcd*]]
            [clojure.math.combinatorics :refer [subsets cartesian-product]]))

(defn sum-numerator
  ([s lcm]
   (apply +' (map #(as-> (quot lcm %) n (*' n n)) s)))
  ([s]
   (sum-numerator s (apply lcm* s))))

(defn gen-info-map
  [limit]
  (let [ps (p/primes (inc (quot limit 3)))
        valid? (fn [p pp]
                 (let [p2 (*' p p)]
                   (->> (subsets (range 1 (inc (quot limit pp))))
                        (drop-while #(< (count %) 2))
                        (map sum-numerator)
                        (some #(zero? (mod % p2))))))
        powers (fn [p]
                 (->> (map #(vector p (expt p %)) (rest (range)))
                      (map #(and (apply valid? %) %))
                      (take-while some?)
                      (map last)))
        pp-seq (->> (map powers ps)
                    (filter not-empty))
        lcm (apply *' (map last pp-seq))
        target (quot (*' lcm lcm) 2)]
    {:lcm lcm
     :limit limit
     :pp-seq pp-seq
     :primes (map first pp-seq)
     :target-set (->> (subsets (first pp-seq))
                      (map #(- target (sum-numerator % lcm)))
                      (into #{}))}))

(defn gen-pp-map
  [info-map [pp md]]
  (let [p (first (p/factors pp))
        {:keys [pp-seq lcm limit]} info-map
        pp-lcm (->> (take-while #(<= (first %) p) pp-seq)
                    (map last)
                    (apply *'))]
    (->> (p/divisors pp-lcm)
         (filter #(and (<= % limit) (= pp (gcd* (expt p 6) %))))
         subsets
         (map #(vector (sum-numerator % lcm) 1))
         (map #(hash-map (mod (first %) (*' md md)) [%]))
         (apply merge-with concat))))

(defn match-pp
  [info-map [pp md] res-map]
  (let [m (gen-pp-map info-map [pp md])]
    (->> (mapcat (fn [[sum s]]
                   (let [dm (*' md md)
                         r (mod sum dm)
                         dr (if (zero? r) 0 (- dm r))]
                     (when-let [ps (m dr)]
                       (->> (cartesian-product [[sum s]] ps)
                            (map (fn [ss] {(apply +' (map first ss))
                                           (apply *' (map last ss))}))))))
                 res-map)
         (apply merge-with +'))))

(defn euler-152-2
  [limit]
  (let [info-map (gen-info-map limit)
        {:keys [pp-seq target-set]} info-map]
    (loop [pp-list (->> (mapcat #(map vector % (reverse %)) (rest pp-seq))
                        reverse)
           result {0 1}]
      (if (empty? pp-list)
        (->> (filter #(target-set (first %)) result)
             (map last)
             (apply +))
        (recur (rest pp-list) (match-pp info-map (first pp-list) result))))))

(defn euler-152-3
  [limit]
  (let [info-map (gen-info-map limit)
        {:keys [pp-seq target-set]} info-map]
    (loop [pp-list (->> (mapcat #(map vector % (reverse %)) (rest pp-seq))
                        reverse)
           result {0 1}]
      (if (empty? pp-list)
        (->> (filter #(target-set (first %)) result)
             (map last)
             (apply +))
        (recur (rest pp-list) (match-pp info-map (first pp-list) result))))))

(comment
  (time (euler-152-2 200))
  )
