(ns caesarhu.clojure-euler.euler-212
  (:require [clojure.math.numeric-tower :refer [expt]]))

(def sk
  (memoize
   (fn [k]
     (if (<= 1 k 55)
       (mod (+ 100003 (- (* 200003 k)) (* 300007 (expt k 3))) 1000000)
       (mod (+ (sk (- k 24)) (sk (- k 55))) 1000000)))))

(defn sn
  [n]
  (let [s (->> (map #(- (* 6 n) %) (reverse (range 6))) (map sk))
        s0 (vec (map #(mod % 10000) (take 3 s)))
        sd (vec (map #(inc (mod % 399)) (take-last 3 s)))]
    (vec (concat s0 sd))))

(def block 399)

(defn dfs
  [cubes f flag pre here]
  (->> (for [i (range f (count here))
             :let [now (cubes (here i))
                   x (max (pre 0) (now 0))
                   y (max (pre 1) (now 1))
                   z (max (pre 2) (now 2))
                   dx (- (min (+ (pre 0) (pre 3)) (+ (now 0) (now 3))) x)
                   dy (- (min (+ (pre 1) (pre 4)) (+ (now 1) (now 4))) y)
                   dz (- (min (+ (pre 2) (pre 5)) (+ (now 2) (now 5))) z)]]
         (if (<= (min dx dy dz) 0) 0
             (+ (* flag dx dy dz) (dfs cubes (inc i) (- flag) [x, y, z, dx, dy, dz] here))))
       (apply +)))

(defn euler-212
  [N]
  (let [cubes (atom [])
        mp (atom {})]
    (doseq [m (range N)
            :let [[a b c x y z] (sn (inc m))]]
      (swap! cubes assoc m [a b c x y z])
      (doseq [i (range (quot a block) (inc (quot (+ a x) block)))
              j (range (quot b block) (inc (quot (+ b y) block)))
              k (range (quot c block) (inc (quot (+ c z) block)))]
        (swap! mp (partial merge-with into) {[i j k] [m]})))
    (->> (for [[pos here] @mp]
           (dfs @cubes 0 1
                [(* (pos 0) block) (* (pos 1) block) (* (pos 2) block) block block block]
                here))
         (apply +))))

(comment
  (time (euler-212 50000))
  )
