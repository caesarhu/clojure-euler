(ns caesarhu.euler-001)

(defn sum-of-numbers
  [limit step]
  (let [length (quot limit step)
        max-step (* step length)]
    (quot (* (+ step max-step) length) 2)))

(defn euler-001
  [limit]
  (+ (sum-of-numbers (dec limit) 3) (sum-of-numbers (dec limit) 5) (- (sum-of-numbers (dec limit) 15))))

(comment
  (time (euler-001 1000))
  )