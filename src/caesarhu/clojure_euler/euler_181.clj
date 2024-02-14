(ns caesarhu.clojure-euler.euler-181
  (:require [clojure.math.numeric-tower :refer [gcd]]))

(defn euler-181
  [black white]
  (let [state (atom (vec (repeat (inc black) (vec (repeat (inc white) 0)))))
        next-state! (fn [b w]
                      (doseq [i (range (inc (- black b)))
                              j (range (inc (- white w)))
                              :let [n (get-in @state [i j])]]
                        (swap! state update-in [(+ i b) (+ j w)] (partial + n))))]
    (swap! state assoc-in [0 0] 1)
    (doseq [b (range (inc black))
            w (range (inc white))
            :when (pos-int? (+ b w))]
      (next-state! b w))
    (get-in @state [black white])))

(comment
  (time (euler-181 60 40))
  )

(defn sum-divisors
  [n-max]
  (let [sigma (atom (vec (repeat (inc n-max) 0)))]
    (doseq [i (range 1 (inc n-max))
            j (range 1 (inc (quot n-max i)))
            :let [idx (* i j)]]
      (cond
        (> i j) (swap! sigma update idx #(+ i j %))
        (= i j) (swap! sigma update idx #(+ i %))))
    @sigma))

(defn count-partitions
  [black white]
  (let [state (atom (vec (repeat (inc black) (vec (repeat (inc white) 0)))))
        sigma (sum-divisors (max black white))]
    (swap! state assoc-in [0 0] 1)
    (doseq [b (range (inc black))
            w (range (inc white))
            :when (pos-int? (+ b w))]
      (doseq [r (range (inc b))
              s (range (inc w))
              :when (pos-int? (+ r s))
              :let [d (gcd r s)]]
        (swap! state update-in [b w]
               (partial + (quot (* (sigma d) (+ r s) (get-in @state [(- b r) (- w s)])) d))))
      (swap! state update-in [b w] #(quot % (+ b w))))
    (get-in @state [black white])))

(comment
  (time (count-partitions 60 40))
  )
