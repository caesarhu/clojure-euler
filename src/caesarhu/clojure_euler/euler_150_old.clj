(ns caesarhu.clojure-euler.euler-150-old
  (:require [clojure.math.numeric-tower :refer [expt]]))

(def mod-base (expt 2 20))
(def minus-base (/ mod-base 2))

(defn next-t
  [t]
  (mod (+ (* 615949 t) 797807) mod-base))

(defn t->s
  [t]
  (- t minus-base))

(defn pseudo-vector
  [height]
  (let [s (map t->s (rest (iterate next-t 0)))]
    (first (reduce (fn [[result s] i]
                     (let [[row other] (split-at i s)]
                       [(conj result (vec row)) other]))
                   [[] s] (range 1 (inc height))))))

(defn euler-150
  [limit]
  (let [triangle (pseudo-vector limit)]
    (loop [i (int (- limit 1))
           res []
           min-so-far (long 99999999)]
      (if (neg? i)
        min-so-far
        (if (== i (- limit 1))
          (let [nres (mapv #(hash-map :all [(get-in triangle [i %])]
                                      :right [(get-in triangle [i %])])
                           (range limit))]
            (recur (- i 1) nres (apply min (map #(first (:all %)) nres))))
          (let [nres (mapv #(let [value (get-in triangle [i %])]
                              (hash-map :all (->> (map (fn [a b] (+ value a b))
                                                       (:all (res %))
                                                       (:right (res (+ % 1))))
                                                  (cons value)
                                                  vec)
                                        :right (->> (map (fn [a] (+ value a))
                                                         (:right (res (+ % 1))))
                                                    (cons value)
                                                    vec)))
                           (range (+ i 1)))]
            (recur (- i 1) nres (->> (map #(apply min (:all %)) res)
                                     (apply min)
                                     (min min-so-far)))))))))

(comment
  (time (euler-150 1000))
  )