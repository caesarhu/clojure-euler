(ns caesarhu.clojure-euler.euler-198)

(defn euler-198
  [N D]
  (let [answer (- (quot N 2) (quot D 2))
        stack (atom (list))
        push (fn [x] (swap! stack #(cons x %)))
        pop (fn [] (let [x (first @stack)]
                     (swap! stack rest)
                     x))
        count-ambiguous (fn [q qx] (quot (- N (* 2 q qx)) (* 2 q q)))]
    (push [[0 1] [1 D] [1 (dec D)]])
    (loop [answer answer]
      (if (empty? @stack)
        answer
        (let [[[pl ql] [p q] [pr qr]] (pop)
              nl (max (count-ambiguous q ql) 0)
              nr (if (> q D) (max (count-ambiguous q qr) 0) 0)]
          (when (pos? nl)
            (push [[pl ql] [(+ p pl) (+ q ql)] [p q]]))
          (when (pos? nr)
            (push [[p q] [(+ p pr) (+ q qr)] [pr qr]]))
          (recur (+ answer nl nr)))))))

(comment
  (time (euler-198 100000000 100))
  )
