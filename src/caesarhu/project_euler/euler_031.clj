(ns caesarhu.project-euler.euler-031)

(def coins (reverse [1 2 5 10 20 50 100 200]))

(defn target-changes
  [result target coins]
  (cond
    (zero? target) [result]
    (empty? coins) nil
    :else (let [coin (first coins)
                amount (quot target coin)]
            (if (= (count coins) 1)
              (let [cs (repeat amount coin)]
                (target-changes (concat result cs) (- target (apply + cs)) (rest coins)))
              (->> (for [i (reverse (range (inc amount)))
                         :let [cs (repeat i coin)]]
                     (target-changes (concat result cs) (- target (apply + cs)) (rest coins)))
                   (apply concat))))))

(defn euler-031
  [target]
  (-> (target-changes [] target coins) count))

(comment
  (time (euler-031 200))
  )