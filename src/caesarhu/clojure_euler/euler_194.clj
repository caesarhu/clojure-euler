(ns caesarhu.clojure-euler.euler-194
  (:require [clojure.math.numeric-tower :refer [expt]]
            [caesarhu.math.math-tools :refer [factorial]]))

(def a-rule {0 [1 2 4] 1 [0 3 6] 2 [0 3 4] 3 [1 2 6] 4 [0 2 5] 5 [4 6] 6 [1 3 5]})
(def b-rule {0 [1 2 4] 1 [0 6] 2 [0 3 4] 3 [2 6] 4 [0 2 5] 5 [4 6] 6 [1 3 5]})

(defn colors
  [c]
  (set (range c)))

(defn -gen-units
  [rule color-sets]
  (let [idx (some #(and (set? (color-sets %)) %) (range (count color-sets)))
        reduce-color (fn [color]
                       (reduce (fn [unit pos]
                                 (if-let [set-or-n (and (set? (unit pos)) (unit pos))]
                                   (assoc unit pos (disj set-or-n color))
                                   unit))
                               (assoc color-sets idx color)
                               (rule idx)))]
    (map reduce-color (color-sets idx))))

(defn gen-units
  [rule color-sets]
  (loop [cs [color-sets]]
    (if (some set? (first cs))
      (recur (mapcat #(-gen-units rule %) cs))
      cs)))

(comment
  (let [s (->> (range 3 11)
               (map #(vector % (count (gen-units a-rule (vec (repeat 7 (colors %))))))))]
    [(vec (map first s)) (vec (map last s))])
  ; [[3 4 5 6 7 8 9 10] [24 744 7440 41880 167160 530544 1429344 3404880]]
  (let [s (->> (range 3 11)
               (map #(vector % (count (gen-units b-rule (vec (repeat 7 (colors %))))))))]
    [(vec (map first s)) (vec (map last s))])
  ; [[3 4 5 6 7 8 9 10] [36 1056 9720 51840 199500 616896 1629936 3824640]]
  )

(defn neville [xs ys n x]
  ((memoize (fn p [i j]
              (if (= i j) (ys i)
                  (/ (- (*' (- x (xs j)) (p i (dec j)))
                        (*' (- x (xs i)) (p (inc i) j)))
                     (- (xs i) (xs j))))))
   0 n))

(defn PA
  [x]
  (neville [3 4 5 6 7 8 9 10]
           [24 744 7440 41880 167160 530544 1429344 3404880]
           7
           x))

(defn PB
  [x]
  (neville [3 4 5 6 7 8 9 10]
           [36 1056 9720 51840 199500 616896 1629936 3824640]
           7
           x))

(defn N
  [a b c]
  (*' (quot (factorial (+' a b)) (*' (factorial a) (factorial b)))
      (quot (*' (expt (PA c) a) (expt (PB c) b))
            (expt (*' c (dec c)) (dec (+ a b))))))

(defn euler-194
  [a b c]
  (-> (mod (N a b c) (expt 10 8)) long))

(comment
  (= (PA 13) (count (gen-units a-rule (vec (repeat 7 (colors 13))))))
  (= (PB 13) (count (gen-units b-rule (vec (repeat 7 (colors 13))))))
  (= 20736 (N 2 2 3))
  (time (euler-194 25 75 1984))
  )
