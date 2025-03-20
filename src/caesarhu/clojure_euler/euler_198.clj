(ns caesarhu.clojure-euler.euler-198)

(defn euler-198
  [N D]
  (let [count-ambiguous (fn [q qx]
                          (max 0 (quot (- N (* 2 q qx)) (* 2 q q))))]
    (loop [answer (- (quot N 2) (quot D 2))
           stack (list [[0 1] [1 D] [1 (dec D)]])]
      (if (not-empty stack)
        (let [[[pl ql] [p q] [pr qr]] (first stack)
              nl (count-ambiguous q ql)
              nr (if (> q D) (count-ambiguous q qr) 0)]
          (recur (+ answer nl nr)
                 (cond->> (rest stack)
                   (pos? nl) (cons [[pl ql] [(+ p pl) (+ q ql)] [p q]])
                   (pos? nr) (cons [[p q] [(+ p pr) (+ q qr)] [pr qr]]))))
        answer))))

(comment
  (time (euler-198 100000000 100)) 
  )
