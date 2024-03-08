(ns caesarhu.clojure-euler.euler-209
  (:require [caesarhu.math.math-tools :refer [digits digits->number]]
            [clojure.math.numeric-tower :refer [expt]]))

(def lucas-numbers-seq
  (lazy-cat [2 1] (map + (rest lucas-numbers-seq) lucas-numbers-seq)))

(defn formula
  [n]
  (let [ds (digits n 2)
        from (concat (repeat (- 6 (count ds)) 0) ds)
        [a b c] from
        x (bit-xor a (bit-and b c))]
    (-> (concat (rest from) [x])
        (digits->number 2))))

(defn euler-209
  [n]
  (let [length (expt 2 n)
        connections (->> (range length) (map formula) vec)
        lucas-number (vec (take length lucas-numbers-seq))]
    (loop [answer 1
           used #{}]
      (if (= length (count used))
        answer
        (let [start (some #(and (not (used %)) %) (range length))
              cycle (->> (iterate connections start) rest
                         (take-while #(not= % start))
                         (cons start))
              cycle-length (count cycle)]
          (recur (*' answer (lucas-number cycle-length))
                 (into used cycle)))))))

(comment
  (time (euler-209 6))
  )
