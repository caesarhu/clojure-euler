(ns caesarhu.clojure-euler.euler-150
  (:require [clojure.math.numeric-tower :refer [expt]]))

(def sk-vector
  (let [base (expt 2 20)]
    (->> (iterate #(mod (+ (* 615949 %) 797807) base) 0)
         rest
         (map #(- % (expt 2 19))))))

(defn sk-triangle
  [rows]
  (->> (reduce (fn [[acc sk] i]
                 (let [[head tail] (split-at i sk)]
                   [(conj acc (vec head)) tail]))
               [[] sk-vector] (range 1 (inc rows)))
       first))

(defn euler-150
  [limit]
  (let [triangle (sk-triangle limit)
        next-map-row (fn [map-row]
                       (let [i (- (count map-row) 2)]
                         (into []
                               (map (fn [j]
                                      (let [value (get-in triangle [i j])
                                            right (map + (repeat value) (:right (map-row (inc j))))]
                                        {:all (->> (map + right (:all (map-row j))) (cons value) vec)
                                         :right (->> right (cons value) vec)})))
                               (range (inc i)))))]
    (loop [map-row (next-map-row (vec (repeat (inc limit) [])))
           result (apply min (last triangle))]
      (if (= 1 (count map-row))
        result
        (let [mp (next-map-row map-row)]
          (recur mp (apply min result (mapcat :all mp))))))))

(comment
  (time (euler-150 1000))
  )
