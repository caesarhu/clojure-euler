(ns caesarhu.clojure-euler.euler-033
  (:require [caesarhu.math.math-tools :as tools]))

(defn curious-fraction? [n d]
  (let [ns (tools/digits n)
        ds (tools/digits d)]
    (and
     (not (zero? (second ds)))
     (= (second ns) (first ds))
     (= (/ n d) (/ (first ns) (second ds))))))

(defn euler-033 [n]
  (denominator (reduce *
                       (for [a (range 10 n)
                             b (range (inc a) n)
                             :when (curious-fraction? a b)]
                         (/ a b)))))

(comment
  (time (euler-033 100))
  )