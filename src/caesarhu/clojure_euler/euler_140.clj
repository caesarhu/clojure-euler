(ns caesarhu.clojure-euler.euler-140
  (:require [caesarhu.math.pell-equation :refer [diop-DN]]))

(def foundations
  (->> (diop-DN 5 44)
       (filter #(every? pos? %))
       (mapcat (fn [[x y]] [[x y] [x (- y)]]))))

(defn euler-140
  [limit]
  (let [D 5
        [p q] (-> (diop-DN D 1) first)
        next-solution (fn [[x y]]
                    [(+' (*' x p) (*' y q D)) (+' (*' x q) (*' y p))])
        solutions (fn [[x y]]
                    (->> (iterate next-solution [x y])
                         (map (fn [[x y]] (/ (-' x 7) 5)))
                         (filter #(and (integer? %) (pos? %)))
                         (take limit)))]
    (->> (mapcat solutions foundations)
         (into (sorted-set))
         (take limit)
         (apply +))))

(comment
  (time (euler-140 30))
  )