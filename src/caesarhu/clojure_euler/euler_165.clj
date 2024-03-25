(ns caesarhu.clojure-euler.euler-165
  (:require [clojure.set :refer [union]]))

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
    (if pos
      (and (line-inner? line-a pos)
           (line-inner? line-b pos)
           pos)
      nil)))

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
       (map vec)
       sort))
(defn swap-line
  [[a b]]
  [b a])

(defn line-sweep
  [lines-origin]
  (loop [lines lines-origin
         head-set #{}
         tail-set (sorted-set)
         result #{}]
    (if-let [line (first lines)]
      (let [[new-head new-tail] (loop [new-head head-set
                                       new-tail tail-set]
                                  (let [tail-line (first new-tail)]
                                    (if (and tail-line
                                             (<= (ffirst tail-line) (ffirst line)))
                                      (recur (disj new-head (swap-line tail-line))
                                             (disj new-tail tail-line))
                                      [new-head new-tail])))]
        (recur (rest lines)
               (conj new-head line)
               (conj new-tail (swap-line line))
               (union result (set-intersection line head-set))))
      (count result))))

(defn euler-165
  [n]
  (line-sweep (sorted-lines n)))

(comment
  (time (euler-165 5000))
  )
