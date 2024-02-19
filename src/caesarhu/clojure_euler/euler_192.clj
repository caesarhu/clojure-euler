(ns caesarhu.clojure-euler.euler-192
  (:require [caesarhu.math.math-tools :refer [sqrt-continued-fraction square?]]
            [clojure.math.numeric-tower :refer [expt ceil]]))

(defn different
  "Returns the approximation error of a given rational approximation to `target`."
  [target numer denom]
  ;; Comparing √x directly to n/d requires arbitrary precision = slow
  ;; Comparing x to (n/d)² is the way to go
  (let [t (/ numer denom)]
    (abs (- target (* t t)))))

(defn closest-denominator
  "Finds the rational number n/d that is the best approximation to √x
  with denominator ≤ max-d, and returns d."
  [x max-d]
  (when-not (square? x)
    (let [cf (sqrt-continued-fraction x)]
      (loop [a-vals (cons (first cf) (cycle (rest cf)))
             n-2 0 n-1 1
             d-2 1 d-1 0]
        (let [a (first a-vals)
              n (+ (* a n-1) n-2)
              d (+ (* a d-1) d-2)]
          ;; if we have exceeded the denominator bound, step back and find the semiconvergents
          ;; watch out for the special condition for the starting min-a when a is even:
          ;; if the semi-convergent at a/2 is a worse approximation than the previous
          ;; full convergent, skip it
          (if (> d max-d)
            (or (last (for [k (range (ceil (/ a 2)) (inc a))
                            :let   [kn (+ (* k n-1) n-2)
                                    kd (+ (* k d-1) d-2)]
                            :when  (if (and (even? a) (= k (/ a 2)))
                                     (< (different x kn kd) (different x n-1 d-1))
                                     true)
                            :while (<= kd max-d)]
                        kd))
                d-1)
            (recur (rest a-vals) n-1 n d-1 d)))))))

(defn euler-192
  "Find the sum of all denominators of the best approximations to √n
  for the denominator bound of 10^12, where n is not a perfect square and
  1 < n ≤ 100000."
  [bound limit]
  (->> (range 2 (inc bound))
       (keep #(closest-denominator % limit))
       (reduce +')))

(comment
  (time (euler-192 (expt 10 5) (expt 10 12)))
  )
