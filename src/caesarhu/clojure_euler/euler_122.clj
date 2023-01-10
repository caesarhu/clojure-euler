(ns caesarhu.clojure-euler.euler-122)

(defn back-track-atom
  [limit]
  (let [cost (atom (vec (cons 0 (repeat limit 10000))))
        path (atom [1])]
    (letfn [(recursive
              [power depth]
              (when (and (<= power limit) (<= depth (@cost power)))
                (swap! cost assoc power depth)
                (swap! path assoc depth power)
                (doseq [d (range depth -1 -1)]
                  (recursive (+ power (@path d)) (inc depth)))))]
      (recursive 1 0))
    @cost))

(defn euler-122-atom
  [limit]
  (->> (back-track-atom limit)
       (apply +)))

;-------------------------------------------------------------------------------------

(defn back-track
  [limit]
  (let [init-cost (vec (cons 0 (repeat limit 1000000)))
        init-path [1]]
    (letfn [(back-cost [[cost path] power depth]
              (if (and (<= power limit) (<= depth (cost power)))
                (let [new-cost (assoc cost power depth)
                      new-path (assoc path depth power)]
                  (back-path [new-cost new-path] power depth))
                [cost path]))
            (back-path [[cost path] power depth]
              (reduce (fn [[new-cost new-path] d]
                        (back-cost [new-cost new-path] (+ power (new-path d)) (inc depth)))
                      [cost path]
                      (range depth -1 -1)))]
      (back-cost [init-cost init-path] 1 0))))

(defn euler-122
  [limit]
  (->> (back-track limit)
       first
       (apply +)))

(comment 
  (peek [1 2 3])
  (time (euler-122 500))
  (time (euler-122-atom 500))
  )
