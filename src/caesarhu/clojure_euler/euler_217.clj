(ns caesarhu.clojure-euler.euler-217
  (:require [clojure.math.combinatorics :as c]
            [clojure.math.numeric-tower :refer [expt]]
            [caesarhu.math.math-tools :refer [digits->number digits]]))

(def M (expt 3 15))

(def default-map (->> (map #(hash-map % (vector (bigint %) 1N)) (range 10)) (apply merge)))

(defn expand-map
  [allow-head-zero? m]
  (let [head-range (if allow-head-zero? (range 10) (range 1 10))
        e (quot (apply max (keys m)) 9)
        e10 (expt 10 e)]
    (->> (for [i head-range
               [k [sum n]] m]
           {(+ k i) [(+ (* i e10 n) sum) n]})
         (apply merge-with #(map + %1 %2)))))

(def digits-map
  (memoize
   (fn [allow-head-zero? n]
     (cond
       (and (= 1 n) allow-head-zero?) default-map
       (and (= 1 n) (not allow-head-zero?)) (dissoc default-map 0)
       :else (expand-map allow-head-zero? (digits-map true (dec n)))))))

(defn count-map-sum
  [n]
  (if (= 1 n) 45
      (let [idx (quot n 2)
            m1 (digits-map false idx)
            m2 (digits-map true idx)]
        (->> (for [[k1 [sum1 n1]] m1]
               (let [[sum2 n2] (m2 k1)
                     sum3 45
                     n3 10]
                 (if (odd? n)
                   (+ (* sum1 n2 n3 (expt 10 (inc idx)))
                      (* sum2 n1 n3)
                      (* sum3 n1 n2 (expt 10 idx)))
                   (+ (* sum1 n2 (expt 10 idx))
                      (* sum2 n1)))))
             (apply +)))))

(defn euler-217-raw
  [target]
  (->> (range 1 (inc target))
       (map count-map-sum)
       (apply +)))

(defn euler-217
  [target]
  (-> (mod (euler-217-raw target) M) long))

(comment
  (time (euler-217 47))
  )
