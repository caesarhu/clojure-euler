(ns caesarhu.project-euler.euler-031
  (:require [caesarhu.kuafu.sat :as sat]))

(def coins [200 100 50 20 10 5 2 1])

(defn changes
  [result target coins]
  (let [coin (first coins)]
    (cond
      (zero? target) [result]
      (= coin 1) [(concat result (repeat target coin))]
      :else (reduce concat
                    (for [i (range (inc (quot target coin)))]
                      (changes (concat result (repeat i coin)) (- target (* coin i)) (rest coins)))))))

(defn euler-031
  [target]
  (-> (changes [] target coins) count))

(comment
  (time (euler-031 200))
  )

(defn kuafu-031
  [n]
  (let [model (sat/cp-model)
        solver (sat/cp-solver)
        coin-vars (map #(sat/int-var model 0 (quot n %)) coins)]
    (sat/add-equality model (sat/weighted-sum coin-vars coins) n)
    (sat/set-all-solutions solver true)
    (reset! sat/*solutions* (list))
    (sat/solve solver model (sat/callback coin-vars))
    (count @sat/*solutions*)))

(comment
  (time (kuafu-031 200))
  )