(ns caesarhu.clojure-euler.euler-220
  (:require [clojure.math.numeric-tower :refer [expt]]
            [clojure.string :as str]))

(def mp (atom {0 [0, 0] 1 [ 0, 1]}))

(defn calculate
  [c p]
  (let [[cx cy] c
        [px py] p]
    [(+ cx (- py) cy) (+ cy px (- cx))]))

(defn find-mp
  [n]
  (if-let [x (@mp n)]
    x
    (let [pw (loop [pw 1]
               (if (< pw n)
                 (recur (* 2 pw))
                 pw))
          x1 (find-mp (quot pw 2))
          x2 (find-mp (- pw n))]
      (swap! mp assoc n (calculate x1 x2))
      (@mp n))))

(defn euler-220
  [target]
  (reset! mp {0 [0, 0] 1 [0, 1]})
  (->> (find-mp target)
       (str/join ",")))

(comment
  (time (euler-220 (expt 10 12)))
  )
