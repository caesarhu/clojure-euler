(ns caesarhu.clojure-euler.euler-122)

(defn back-track
  [limit]
  (let [cost (atom (vec (cons 0 (repeat limit limit))))
        path (atom [])]
    (letfn [(track
              [power depth]
              (when (and (<= power limit) (<= depth (@cost power)))
                (swap! cost assoc power depth)
                (swap! path assoc depth power)
                (doseq [d (range depth -1 -1)]
                  (track (+ power (@path d)) (inc depth)))))]
      (track 1 0))
    @cost))

(defn euler-122
  [limit]
  (->> (back-track limit)
       (apply +)))

(comment
  (time (euler-122 200))
  )
