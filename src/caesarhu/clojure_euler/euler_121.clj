(ns caesarhu.clojure-euler.euler-121)

(defn next-row
  [row]
  (let [probability (/ 1 (inc (count row)))
        prob-blue (map #(* probability %) row)
        prob-red  (map #(* (- 1 probability) %) row)]
    (map + (concat prob-blue [0]) (cons 0 prob-red))))

(def triangle
  (iterate next-row (list 1)))

(defn euler-121
  [turns]
  (->> (nth triangle turns)
       (take (+ (quot turns 2) (mod turns 2)))
       (apply +)
       (/ 1)
       bigint))

(comment
  (take 10 triangle)
  (euler-121 15)
  )