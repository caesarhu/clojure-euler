(ns caesarhu.project-euler.euler-028)

(defn corners
  [n]
  (if (= n 1) [1]
      (let [square (* n n)]
        (take 4 (iterate #(- % (dec n)) square)))))

(defn euler-028
  [limit]
  (->> (range (inc limit))
       (filter odd?)
       (mapcat corners)
       (apply +)))

(comment
  (time (euler-028 1001))
  )