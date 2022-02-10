(ns caesarhu.project-euler.euler-079
  (:require [caesarhu.math.math-tools :as tools]))

(def keylog
  (->> (slurp "resources/p079_keylog.txt")
       (clojure.string/split-lines)
       (map #(Long/parseLong %))
       (map tools/digits)))

(defn key-digits
  [s]
  (let [digits (reduce (fn [acc log]
                         (clojure.set/union acc (set log)))
                       #{} s)]
    (-> digits sort vec)))

(defn switch-digit
  [guess x y]
  (let [g-map (->> (map-indexed (fn [itx itm] [itm itx]) guess) (into {}))]
    (if (> (g-map x) (g-map y))
      (mapcat #(if (= % x) [x y] [%]) (remove #(= % y) guess))
      guess)))

(defn log-process
  [guess log]
  (reduce (fn [acc pair]
            (apply switch-digit acc pair))
          guess (partition 2 1 log)))

(defn euler-079
  []
  (->> (reduce (fn [acc log]
                 (log-process acc log))
               (key-digits keylog) keylog)
       tools/digits->number))

(comment
  (time (euler-079))
  )