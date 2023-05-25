(ns caesarhu.clojure-euler.euler-152-2
  (:require [clojure.math.combinatorics :refer [subsets cartesian-product]]
            [clojure.math.numeric-tower :refer [expt]]
            [caesarhu.math.primes :as p]
            [caesarhu.math.math-tools :refer [lcm* gcd*]]))

(def LCM (atom (* 2 2 2 3 3 5 7 13)))

(defn square
  [n]
  (*' n n))

(defn sum-numerator
  [s]
  (apply +' (map #(square (quot @LCM %)) s)))

(defn valid?
  [s p]
  (let [n (sum-numerator s)
        gcd (gcd* n (square @LCM))]
    (zero? (mod (quot n gcd) (square p)))))

(defn valid-power?
  [limit p pp]
  (->> (subsets (range 1 (inc (quot limit pp))))
       (drop-while #(< (count %) 2))
       (some #(valid? % p))))

(defn power-seq
  [limit]
  (let [max-power (fn [p] (->> (iterate inc 1)
                               (take-while #(true? (valid-power? limit p (expt p %))))
                               last))]
    (->> (p/primes (inc (quot limit 3)))
         (map #(vector % (max-power %)))
         (filter #(some? (last %))))))

(defn euler-152
  [limit]
  (let [p-seq (power-seq limit)
        _ (->> (map #(apply expt %) p-seq) (apply *') (reset! LCM))
        target (quot (square @LCM) 2)
        target-map (->> (range (-> p-seq first last) 0 -1)
                        (map #(expt 2 %))
                        subsets
                        (map #(vector (- target (sum-numerator %)) %))
                        (into {}))
        max-power (fn [p] (some #(and (= p (first %)) (last %)) p-seq))
        mod-power (fn [prime power] (expt prime (inc (- (max-power prime) power))))
        gen-prime-map (fn [prime power]
                        (let [pseq (->> (take-while #(<= (first %) prime) p-seq)
                                        (map #(apply expt %)))
                              top (last pseq)
                              pp (expt prime power)]
                          (->> (p/divisors (apply *' pseq))
                               (filter #(and (= pp (gcd* % top)) (<= % limit)))
                               subsets
                               (map #(vector (sum-numerator %) %))
                               (map #(hash-map (mod (first %) (square (mod-power prime power))) [%]))
                               (apply merge-with concat))))]
    (loop [[[prime power] & more :as seq] (reverse p-seq)
           result [[0 []]]]
      (if (<= prime 2)
        (->> (map (fn [[sum s]]
                    (when-let [t (target-map sum)]
                      (concat s t))) result)
             (remove nil?)
             count)
        (let [m (gen-prime-map prime power)
              new-result (mapcat (fn [[sum s]]
                                   (let [ds (square (mod-power prime power))
                                         r (mod sum ds)
                                         dr (if (zero? r) 0 (- ds r))]
                                     (when-let [ps (m dr)]
                                       (->> (cartesian-product [[sum s]] ps)
                                            (map (fn [ss] [(apply +' (map first ss))
                                                           (apply concat (map last ss))])))))) result)]
          (if (<= power 1)
            (recur more new-result)
            (recur (cons [prime (dec power)] more) new-result)))))))

(comment
  (power-seq 140)
  (time (euler-152 110))
  )