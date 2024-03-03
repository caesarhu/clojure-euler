(ns caesarhu.clojure-euler.euler-202
  (:require [clojure.math.numeric-tower :refer [expt gcd]]
            [caesarhu.math.primes :as p]))

(defn euler-202-slow
  [N]
  (let [n (quot (+ N 3) 2)
        offset (mod (* 2 n) 3)]
    (->> (for [k (range 0 (+ (quot (- (quot n 2) offset) 3) 1))
               :when (= 1 (gcd n (+ (* 3 k) offset)))]
           1)
         (apply +)
         (* 2))))

(defn euler202-helper
  [z pVec]
  (let [p-length (count pVec)
        L (expt 2 p-length)
        [dVec mVec] (loop [i 1 dVec [1] mVec [1] pVec pVec]
                      (if (empty? pVec)
                        [dVec mVec]
                        (let [p (first pVec)]
                          (recur (* i 2)
                                 (into dVec (map #(* p %) (subvec dVec 0 i)))
                                 (into mVec (map - (subvec mVec 0 i)))
                                 (rest pVec)))))]
    (->> (for [i (range L)
               :let [x (quot z (dVec i))
                     t (+ (quot (dec x) 3) (if (= 2 (mod x 3)) 1 0))]]
           (* (mVec i) t))
         (apply +))))

(defn euler-202
  [n]
  (when (= 1 (mod n 2))
    (let [z (quot (+ n 3) 2)
          pVec (->> (p/factors z) frequencies keys sort)]
      (euler202-helper z pVec))))

(comment
  (time (euler-202-slow 1000001))
  (time (euler-202 12017639147))
  )
