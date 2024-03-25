(ns caesarhu.clojure-euler.euler-165
  (:require [clojure.set :refer [union]]
            [clojure.data.priority-map :refer [priority-map]]))

(def blum-init [290797 0])

(defn next-blum
  [[s _]]
  (let [next-s (mod (* s s) 50515093)]
    [next-s (mod next-s 500)]))

(defn lines
  [n]
  (->> (iterate next-blum (next-blum blum-init))
       (take (* 4 n))
       (map last)
       (partition 2)
       (partition 2)))

(defn formula
  [[[x1 y1] [x2 y2]]]
  (let [a (- y2 y1)
        b (- x1 x2)
        c (+ (* a x1) (* b y1))
        result [a b c]]
    (if (neg? a)
      (vec (map - result))
      result)))

(defn intersection
  [f-a f-b]
  (let [[a b c] f-a
        [d e f] f-b
        den (- (* a e) (* b d))]
    (when (not (zero? den))
      [(/ (- (* c e) (* b f)) den)
       (/ (- (* a f) (* c d)) den)])))

(defn between?
  [pair x]
  (let [[a b] (sort pair)]
    (< a x b)))

(defn line-inner?
  [line [x y]]
  (let [x-range (map first line)
        y-range (map last line)
        x? (or (apply = x x-range) (between? x-range x))
        y? (or (apply = y y-range) (between? y-range y))]
    (and x? y?)))

(defn true-intersection
  [line-a line-b]
  (let [pos (intersection (formula line-a) (formula line-b))]
    (when pos
      (and (line-inner? line-a pos)
           (line-inner? line-b pos)
           pos))))

(defn set-intersection
  [line other-lines]
  (set (for [other other-lines
             :let [pos (true-intersection line other)]
             :when pos]
         pos)))

(defn sorted-lines
  [n]
  (->> (lines n)
       (map (fn [[a b]]
              (sort [(vec a) (vec b)])))
       (map vec)))

(defn sweep
  [lines]
  (let [lines (vec lines)
        x-lines (->> (map #(apply map vector %) lines) (map first)
                     (map vector (range (count lines))) (into (priority-map)))]
    (loop [x-lines x-lines
           target-x (priority-map)
           y-head (priority-map)
           y-tail (priority-map)
           result #{}]
      (if (empty? x-lines)
        result
        (let [[k [x0 x1] :as line] (first x-lines)
              drop-keys (->> (subseq target-x <= x0) (map first))
              new-target-x (apply dissoc target-x drop-keys)
              new-y-head (apply dissoc y-head drop-keys)
              new-y-tail (apply dissoc y-tail drop-keys)
              [y0 y1] (->> (lines k) (apply map vector) last sort vec)
              drop-y-keys (->> (subseq new-y-tail <= y0) (map key))
              target (->> (subseq (apply dissoc new-y-head drop-y-keys) < y1)
                          (map key))
              intersections (set-intersection (lines k) (map lines target))]
          (recur (rest x-lines)
                 (merge new-target-x {k x1})
                 (merge new-y-head {k y0})
                 (merge new-y-tail {k y1})
                 (union result intersections)))))))

(defn euler-165
  [n]
  (->> (sweep (sorted-lines n)) count))

(comment
  (time (euler-165 5000))
  )
