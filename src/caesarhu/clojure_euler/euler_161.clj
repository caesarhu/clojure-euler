(ns caesarhu.clojure-euler.euler-161
  (:require [clojure.math.numeric-tower :refer [expt]]))

(def tiles
  [[2 0 0 0] [0 2 0 0] [0 0 2 0] [0 0 0 2]
   [1 1 0 0] [0 1 1 0] [0 0 1 1] [1 0 0 1] [1 0 1 0] [0 1 0 1]])

(defn match-tiles
  [tiles, nstates, x, y, count, is-bottom?,is-right?]
  (let [above (mod x 3)
        left (mod (quot x (expt 3 y)) 3)]
    (apply merge-with +
           nstates
           (for [t tiles
                 :when (and (zero? (mod (+ (get t 0) above) 3))
                            (zero? (mod (+ (get t 3) left) 3))
                            (or (not is-bottom?) (zero? (get t 2)))
                            (or (not is-right?) (zero? (get t 1))))
                 :let [n (+ x (- above) (get t 2) (- (* left (expt 3 y))) (* (get t 1) (expt 3 y)))]]
             {n count}))))

(defn euler-161
  [height width]
  (let [states (atom {0 1})]
    (doseq [i (range width)
            j (range height)
            :let [is-bottom? (= (inc j) height)
                  is-right? (= (inc i) width)
                  current-states (reduce (fn [nstates [k v]]
                                           (match-tiles tiles nstates k (inc j) v is-bottom? is-right?))
                                         {} @states)]]
      (reset! states current-states))
    (@states 0)))

(comment
  (time (euler-161 9 12))
  )
