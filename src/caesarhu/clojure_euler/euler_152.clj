(ns caesarhu.clojure-euler.euler-152
  (:require [caesarhu.math.primes :as p]
            [clojure.math.numeric-tower :refer [expt]]
            [caesarhu.math.math-tools :refer [lcm* gcd*]]
            [clojure.math.combinatorics :refer [subsets cartesian-product]]))

(defn sum-numerator
  ([s lcm]
   (apply +' (map #(as-> (quot lcm %) n (*' n n)) s)))
  ([s]
   (sum-numerator s (apply lcm* s))))

(defn prime-and-power
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
                      (map last)))]
    (->> (map powers ps)
         (filter not-empty))))

(defn euler-152
  [limit]
  (let [pp-seq (prime-and-power limit)
        lcm (apply *' (map last pp-seq))
        target (quot (*' lcm lcm) 2)
        target-map (->> (subsets (first pp-seq))
                        (map #(vector (- target (sum-numerator % lcm)) %))
                        (into {}))
        gen-pp-map (fn [pp md]
                     (let [p (first (p/factors pp))
                           pp-lcm (->> (take-while #(<= (first %) p) pp-seq)
                                       (map last)
                                       (apply *'))]
                       (->> (p/divisors pp-lcm)
                            (filter #(and (<= % limit) (= pp (gcd* (expt p 6) %))))
                            subsets
                            (map #(vector (sum-numerator % lcm) %))
                            (map #(hash-map (mod (first %) (*' md md)) [%]))
                            (apply merge-with concat))))]
    (loop [pps (->> (mapcat #(map vector % (reverse %)) (rest pp-seq))
                    reverse)
           result [[0 []]]]
      (if (empty? pps)
        (->> (map (fn [[sum s]]
                    (when-let [t (target-map sum)]
                      (concat s t)))
                  result)
             (remove nil?)
             count)
        (let [[pp md] (first pps)
              m (gen-pp-map pp md)
              new-result (mapcat (fn [[sum s]]
                                   (let [dm (*' md md)
                                         r (mod sum dm)
                                         dr (if (zero? r) 0 (- dm r))]
                                     (when-let [ps (m dr)]
                                       (->> (cartesian-product [[sum s]] ps)
                                            (map (fn [ss] [(apply +' (map first ss))
                                                           (apply concat (map last ss))]))))))
                                 result)]
          (recur (rest pps) new-result))))))

(comment
  (time (euler-152 100))
  )