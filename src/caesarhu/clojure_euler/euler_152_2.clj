(ns caesarhu.clojure-euler.euler-152-2
  (:require [caesarhu.math.primes :as p]
            [clojure.math.numeric-tower :refer [expt]]
            [caesarhu.math.math-tools :refer [lcm* gcd* coprime?]]
            [clojure.math.combinatorics :refer [subsets cartesian-product]]
            [clojure.core.reducers :as r]))

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
     :target-set (->> (first pp-seq)
                      subsets
                      (map #(- target (sum-numerator % lcm))))}))

(defn gen-pp-map
  [info-map [pp md]]
  (let [{:keys [pp-seq lcm limit]} info-map
        pp-lcm (->> (map last pp-seq)
                    (take-while #(coprime? % pp))
                    (apply *')
                    (quot lcm))
        pp-sets (->> (range 1 (inc (quot limit pp)))
                     (map #(*' pp %))
                     (filter #(and (zero? (mod lcm %)) (= pp (gcd* % pp-lcm))))
                     subsets)]
    (transduce (comp
                (map #(vector (sum-numerator % lcm) 1))
                (map #(hash-map (mod (first %) (*' md md)) [%])))
               (partial merge-with concat)
               {} pp-sets)))

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
        {:keys [pp-seq target-set]} info-map
        pp-list (->> (mapcat #(map vector % (reverse %)) (rest pp-seq))
                     reverse)
        pp-map (reduce #(match-pp info-map %2 %1) {0 1} pp-list)]
    (->> (map #(pp-map %) target-set)
         (remove nil?)
         (apply +))))

(comment
  (time (euler-152-2 80))
  )
