(ns caesarhu.project-euler.euler-009)

(defn square
  [n]
  (* n n))

(defn euler-009
  [length]
  (first (for [a (range 1 (quot length 3))
               b (range (inc a) (quot (- length a) 2))
               :let [c (- length a b)]
               :when (and (< a b c)
                          (> (+ a b) c)
                          (= (+ (square a) (square b)) (square c)))]
           [(* a b c) [a b c]])))

(comment
  (time (euler-009 1000))
  )