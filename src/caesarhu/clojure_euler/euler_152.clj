(ns caesarhu.clojure-euler.euler-152
  (:require [caesarhu.math.math-tools :refer [coprime?]]
            [clojure.math.combinatorics :refer [subsets cartesian-product]]
            [caesarhu.math.primes :as p]
            [clojure.set :as set]))

(defn inverse-sq
  [s]
  (transduce (map #(/ (* % %))) + s))

(defn gen-seq
  [n illegal]
  (->> (range 1 (inc n))
       (filter #(coprime? illegal %))
       (subsets)))

(defn prime-sets
  ([limit prime]
   (prime-sets limit prime 1))
  ([limit prime illegal]
   (sequence (comp
              (map #(map (partial * prime) %))
              (filter #(coprime? (denominator (inverse-sq %)) prime)))
             (drop 2 (gen-seq (quot limit prime) illegal)))))

(defn euler-152
  [limit]
  (let [[valid invalid] (vals (group-by #(some? (first (prime-sets limit %)))
                                        (p/primes 5 (inc (quot limit 5)))))
        target (->> (p/divisors (* 2 2 2 3 3)) rest (filter #(<= % limit))
                    subsets rest
                    (map #(hash-map (inverse-sq %) [%]))
                    (apply merge-with concat))
        match-target (fn [s]
                       (when-let [ans (->> (inverse-sq s) (- 1/2) target)]
                         (map #(concat % s) ans)))
        illegal (apply * 1 invalid)
        result (atom #{})]
    (loop [[prime & more] (reverse valid)
           legal 1
           raw-sets [[]]]
      (if (nil? prime)
        (doseq [seq (mapcat match-target raw-sets)]
          (swap! result #(conj % (set seq))))
        (recur more
               (* legal prime)
               (sequence (comp
                          (map #(distinct (apply concat %)))
                          (filter #(or (empty? %)
                                       (coprime? (* legal prime) (denominator (inverse-sq %))))))
                         (cartesian-product (cons [] (prime-sets limit prime illegal))
                                            raw-sets)))))
    (count target)))

(comment
  (->> (prime-sets 80 9))
  (time (euler-152 80))
  )