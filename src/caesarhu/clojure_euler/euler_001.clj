(ns caesarhu.clojure-euler.euler-001
  (:require [caesarhu.kuafu.sat :as sat]))

(defn sum-of-numbers
  [limit step]
  (let [length (quot limit step)
        max-step (* step length)]
    (quot (* (+ step max-step) length) 2)))

(defn euler-001
  "以等差數列公式計算本題最快。"
  [limit]
  (+ (sum-of-numbers (dec limit) 3) (sum-of-numbers (dec limit) 5) (- (sum-of-numbers (dec limit) 15))))

(comment 
  (time (euler-001 1000000000))
  (time (euler-001 1000))
  )

(defn kuafu-001
  "測試用ortools解題"
  [n]
  (let [model (sat/cp-model)
        x (sat/int-var model 1 (dec n) "x")
        y (sat/int-var model (sat/from-values [3 5]) "y")
        solver (sat/cp-solver)]
    (reset! sat/*solutions* (list))
    (sat/add-modulo-equality model (sat/int-var model 0) x y)
    (sat/set-all-solutions solver true)
    (sat/solve solver model (sat/callback x))
    (->> @sat/*solutions*
         set
         (reduce +))))

(comment 
  (time (kuafu-001 1000))
  )