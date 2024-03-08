(ns caesarhu.clojure-euler.euler-208
  (:require [clojure.math :as m]))

(def ways
  (memoize
   (fn [N b l-counts r-counts f]
     (if (and (every? zero? l-counts) (every? zero? r-counts)) 1
         (let [nl_counts (update l-counts f dec)
               nr_counts (update r-counts f dec)]
           (cond-> 0
             (pos-int? (l-counts f)) (+ (ways N b nl_counts r-counts (mod (inc f) 5)))
             (pos-int? (r-counts f)) (+ (ways N b l-counts nr_counts (mod (dec f) 5)))))))))

(defn euler-208
  [N]
  (when (zero? (mod N 5))
    (->> (for [b (range (mod N 2) (inc (quot N 5)) 2)]
           (let [lc (quot (+ N (* b 5)) 10)
                 l-counts (vec (repeat 5 lc))
                 rc (quot (- N (* b 5)) 10)
                 r-counts (vec (repeat 5 rc))
                 res (ways N b l-counts r-counts 0)]
             (if (zero? b) res
                 (*' 2 res))))
         (apply +'))))

(comment
  (time (euler-208 70))
  )
