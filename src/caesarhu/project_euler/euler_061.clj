(ns caesarhu.project-euler.euler-061
  (:require [caesarhu.math.polynomial :as poly]))

(defn polygon
  ([a b c q]
   (let [f1 (poly/quadratic a b c)
         f2 #(quot % q)]
     (comp f2 f1)))
  ([a b c]
   (polygon a b c 1)))

(def polygon-fns
  (map #(apply polygon %)
       [[1 1 0 2]
        [1 0 0]
        [3 -1 0 2]
        [2 -1 0]
        [5 -3 0 2]
        [3 -2 0]]))

(defn seperate
  [n]
  [(quot n 100) (mod n 100)])

(defn take-digits
  [f]
  (->> (map f (iterate inc 1))
       (drop-while #(< % 1000))
       (take-while #(< % 10000))
       (map (fn [n]
              (let [[x y] (seperate n)]
                {x [y]})))
       (apply merge-with concat)))

(defn generate-polygon-map
  []
  (->> (for [i (range (count polygon-fns))]
         {i (take-digits (nth polygon-fns i))})
       (apply merge)))

(def polygon-map (generate-polygon-map))

(defn next-f
  [xs]
  (let [fs (map first xs)
        m (apply dissoc polygon-map fs)
        head (-> xs last last)
        tails ((polygon-map (-> xs last first)) head)]
    (if (empty? m)
      (when (some #(= (-> xs first last) %) tails)
        xs)
      (for [tail tails
            k (keys m)
            :let [thead ((m k) tail)]
            :when thead]
        (conj xs [k tail])))))

(defn euler-061
  []
  (loop [i 6
         result (->> (keys (polygon-map 5))
                     (map #(vector [5 %])))]
    (if (zero? i)
      (let [cycle (map last result)]
        (->> (partition 2 1 (cons (last cycle) cycle))
             (map #(+ (* (first %) 100) (last %)))
             (apply +)))
      (recur (dec i) (mapcat next-f result)))))

(comment
  (time (euler-061))
  )