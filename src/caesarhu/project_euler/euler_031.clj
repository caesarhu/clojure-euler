(ns caesarhu.project-euler.euler-031)

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