(ns caesarhu.clojure-euler.euler-210
  (:require [clojure.math.numeric-tower :refer [expt sqrt ceil floor]]
            [caesarhu.math.polynomial :as p]))

(defn count-all
  "計算所有|x|+|y|=r的整數座標點"
  [r]
  (-> (* r (inc r)) (quot 2) (* 4) inc))

(defn parallelogram-points
  "內部無法形成純角的所有點，先算平行四邊形，再算2個3角形"
  [r]
  (long (+ (* (+ (* r 1/2) 1) (+ (* r 3/4) 1))
           (* r 1/2 r 1/4))))

(defn diag-points
  "x=y的點"
  [r]
  (inc r))

(defn circle-root
  "計算y=k時，與圓相交的2點x座標"
  [r y]
  (let [t (/ r 8)
        d2 (*' 2 t t)
        y2 (*' (- y t) (- y t))]
    (p/quadratic-root 1 (- (/ r 4)) (+ y2 (* t t) (- d2)))))

(defn count-circle
  "計算可形成純角的圓形內的點，包含x=y的點，只減去內切4邊形上的2點"
  [r]
  (let [[x y :as oo] [(/ r 8) (/ r 8)]
        cr (sqrt (+ (* x x) (* y y)))
        box-length (inc (/ r 4))
        box (- (* box-length box-length) 2)
        count-line (fn [s]
                     (let [min-x (-> (apply min s) floor)
                           max-x (-> (apply max s) ceil)]
                       (-> (- max-x min-x) dec long)))]
    (->> (range (inc (/ r 4)) (+ cr y))
         (map #(circle-root r %))
         (map count-line)
         (apply +)
         (* 4)
         (+ box))))

(defn euler-210
  [r]
  (+ (count-circle r) (- (count-all r) (parallelogram-points r) (diag-points r))))

(comment
  (time (euler-210 (expt 10 9)))
  )
