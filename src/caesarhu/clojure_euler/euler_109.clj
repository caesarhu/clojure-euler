(ns caesarhu.clojure-euler.euler-109)

(def single-darts (concat (range 1 21) [25]))
(def double-darts (map #(* 2 %) single-darts))
(def triple-darts (map #(* 3 %) (butlast single-darts)))
(def all-darts (sort (concat single-darts double-darts triple-darts)))

(defn euler-109
  [limit]
  (let [double-seq (filter #(< % limit) double-darts)
        miss-1 (count (for [d double-seq
                            i all-darts
                            :when (< (+ d i) limit)]
                        1))
        miss-0 (count (for [d double-seq
                            i (range (count all-darts))
                            j (range i (count all-darts))
                            :let [ix (nth all-darts i)
                                  jx (nth all-darts j)]
                            :when (< (+ ix jx d) limit)]
                        1))]
    (+ (count double-seq) miss-1 miss-0)))

(comment
  (time (euler-109 100))
  )