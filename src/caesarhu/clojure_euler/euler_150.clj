(ns caesarhu.clojure-euler.euler-150
  (:require [clojure.math.numeric-tower :refer [expt]]
            [net.cgrand.xforms :as x]
            [net.cgrand.xforms.rfs :as rf]))

(def sk-vector
  (let [base (expt 2 20)]
    (->> (iterate #(mod (+ (* 615949 %) 797807) base) 0)
         rest
         (map #(- % (expt 2 19))))))

(defn sum-triangle
  [rows]
  (->> (reduce (fn [[acc sk] i]
                 (let [[head tail] (split-at i sk)]
                   [(conj acc (vec head)) tail]))
               [[] sk-vector] (range 1 (inc rows)))
       first
       (map #(->> (reductions + %) vec))
       vec))

(defn euler-150
  [rows]
  (let [origin (sum-triangle rows)
        value (fn [[i j]] (if-let [v (get-in origin [i j])] v 0))
        min-triangle (fn [[i j]]
                       (->> (for [x (range i rows)]
                              (let [y0 j
                                    y1 (- x (- i j))]
                                (- (value [x y1]) (value [x (dec y0)]))))
                            (reductions +)
                            (apply min)))]
    (->> (for [i (range rows)
               j (range (inc i))]
           (min-triangle [i j]))
         (apply min))))

(defn euler-150-xforms
  [rows]
  (let [origin (sum-triangle rows)
        value (fn [[i j]] (if-let [v (get-in origin [i j])] v 0))
        min-triangle (fn [[i j]]
                       (transduce (comp
                                   (map (fn [x]
                                          (let [y0 j
                                                y1 (- x (- i j))]
                                            (- (value [x y1]) (value [x (dec y0)])))))
                                   (x/reductions +))
                                  rf/min
                                  (range i rows)))]
    (transduce (comp
                (mapcat (fn [i]
                          (map #(vector i %) (range (inc i)))))
                (map min-triangle))
               rf/min
               (range rows))))

(comment
  (time (euler-150 1000))
  (time (euler-150-xforms 1000))
  )
