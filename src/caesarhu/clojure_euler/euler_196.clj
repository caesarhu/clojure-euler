(ns caesarhu.clojure-euler.euler-196
  (:require [caesarhu.math.primes :as p]
            [caesarhu.math.polynomial :refer [quadratic-root]]
            [clojure.set :refer [union]]))

(defn root
  [n]
  (->> (quadratic-root 1 1 (- (* 2 n)))
       (some #(and (pos? %) %))
       long))

(defn quadratic
  [x]
  (quot (* x (inc x)) 2))

(defn number->pos
  [n]
  (let [line (root n)
        sum (quadratic line)]
    (if (= n sum)
      [line (dec line)]
      [(inc line) (- n sum 1)])))

(defn pos->number
  [[x y :as pos]]
  (+ (quadratic (dec x)) (inc y)))

(defn is-prime?
  "Determines if a given integer is prime."
  [x]
  (let [test-primes #{2 3 5 7 11 13 17 19 23 29}]
    (cond
      (= x 1) false
      (test-primes x) true
      (some true? (map #(zero? (mod x %)) test-primes)) false
      :else (p/probable-prime? x))))

(defn line-prime-numbers
  [line]
  (let [t (pos->number [line 0])
        begin (if (even? t) (inc t) t)
        end (pos->number [(inc line) 0])]
    (filter is-prime? (range begin end 2))))

(defn neighbours
  [[x y :as pos]]
  (let [legal-pos? (fn [[x y]] (<= 0 y (dec x)))
        term [-1 0 1]]
    (->> (for [i term
               j term
               :when (not= [0 0] [i j])]
           [(+ x i) (+ y j)])
         (filter legal-pos?))))

(defn prime-triplet
  [prime]
  (let [pos (number->pos prime)
        pos-neighbours (neighbours pos)
        neighbours-prime (->> (map pos->number pos-neighbours)
                              (filter is-prime?))]
    (cond
      (>= (count neighbours-prime) 2) prime
      (zero? (count neighbours-prime)) 0
      :else (let [pos2 (number->pos (first neighbours-prime))
                  pos2-neighbours (set (neighbours pos2))
                  disj-neighbours (apply disj pos2-neighbours (cons pos (neighbours pos)))
                  one? (some is-prime? (map pos->number disj-neighbours))]
              (if one? prime 0)))))

(defn line-sum
  [line]
  (->> (line-prime-numbers line)
       (map prime-triplet)
       (apply +')))

(defn euler-196
  [& more]
  (->> (map line-sum more)
       (apply +')))

(comment
  (time (euler-196 5678027 7208785))
  )
