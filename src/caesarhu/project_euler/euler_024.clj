(ns caesarhu.project-euler.euler-024
  (:require [clojure.math.combinatorics :as combo]
            [caesarhu.math.math-tools :as math]))

(defn solve
  "using clojure.math.combinatorics lib"
  [n]
  (->> (combo/nth-permutation (range 10) (dec n))
       (apply str)))

(defn catch-nth
  [n coll]
  [(nth coll n) (concat (take n coll) (drop (inc n) coll))])

(defn -permutation
  [v result]
  (if (empty? v)
    (apply str result)
    (for [i (range (count v))]
      (let [[x next-v] (catch-nth i v)]
        (-permutation next-v (conj result x))))))

(defn permutation
  [v]
  (flatten (-permutation v [])))

(defn euler-024-slow
  "permutation is very slow"
  [index]
  (nth (permutation (range 10)) (dec index)))

(defn factorial-qmod
  [n]
  (let [f (-> (take-while #(<= (math/factorial %) n) (range)) last)
        q (quot n (math/factorial f))]
    [[q f] (mod n (math/factorial f))]))

(defn factorial-number
  [n]
  (loop [r n
         result []]
    (if (< r (math/factorial 4))
      (conj result [r 0])
      (let [[qf next-r] (factorial-qmod r)]
        (recur next-r (conj result qf))))))

(defn fn->n
  [v [q f]]
  (if (zero? f)
    [(nth (permutation v) (dec q)) []]
    (let [length (count v)
          pre-head (if (> length (inc f))
                     (vec (take (- length (inc f)) v))
                     [])
          [head next-v] (catch-nth q (take-last (inc f) v))]
      [(conj pre-head head) next-v])))

(defn euler-024
  "like clojure.math.combinatorics lib,
   math way is fastest."
  [index]
  (loop [v (range 10)
         fv (factorial-number index)
         result []]
    (if (empty? fv)
      (->> result flatten (apply str))
      (let [[head next-v] (fn->n v (first fv))]
        (recur next-v (rest fv) (conj result head))))))

(comment
  (time (euler-024 10000))
  (time (euler-024-slow 100))
  (time (solve 10000))
  )