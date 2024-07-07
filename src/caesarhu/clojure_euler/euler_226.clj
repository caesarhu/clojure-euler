(ns caesarhu.clojure-euler.euler-226
  (:require [clojure.math :as math :refer [sqrt acos]]
            [clojure.math.numeric-tower :refer [expt round]]
            [caesarhu.math.math-tools :refer [divmod]]))

(def bound 52)

(defn modf
  [x]
  [(- x (long x)) (long x)])

(defn f
  [x]
  (loop [i 0 d 1 x x ans 0]
    (if (>= i bound)
      ans
      (if (<= x 0.5)
        (recur (inc i) (* d 2) (first (modf (* 2 x))) (+ ans (/ x d)))
        (recur (inc i) (* d 2) (first (modf (* 2 x))) (+ ans (/ (- 1 x) d)))))))

(defn c
  [x]
  (- 1/2 (sqrt (- (expt 1/4 2) (expt (- x 1/4) 2)))))

(defn euler-226
  []
  (let [get-mid (fn [low high] (float (/ (+ low high) 2)))
        [low high mid] (loop [low 0
                              high 0.25
                              mid (get-mid low high)]
                         (if (and (not= mid high) (not= mid low))
                           (if (< (c mid) (f mid))
                             (recur low mid (get-mid low mid))
                             (recur mid high (get-mid mid high)))
                           [low high mid]))
        underCircle (- (* 4 (sqrt (- (* 2 (expt mid 3)) (* 4 (expt mid 4)))))
                       (* 8 mid)
                       (sqrt (- (* 2 mid) (* 4 (expt mid 2)))))
        underCircle (/ (+ underCircle (- 4 (acos (sqrt (* 2 mid))))) 16)]
    (loop [ix 1
           area 0]
      (if (>= ix bound)
        (-> (- area underCircle) (* (expt 10 8)) round (* 0.00000001))
        (let [[section, partial] (divmod mid (/ (expt 2 ix)))
              fullSections (- (expt 2 (dec ix)) section 1)
              raw-area (+ area (/ fullSections (expt 2 (inc (* 2 ix)))))
              new-area (if (even? (long section))
                         (+ raw-area (/ (* (- (/ (expt 2 ix)) partial) (+ (/ (expt 2 ix)) partial)) 2))
                         (+ raw-area (/ (* (- (/ (expt 2 ix)) partial) (- (/ (expt 2 ix)) partial)) 2)))]
          (recur (inc ix) new-area))))))

(comment
  (time (euler-226))
  )
