(ns caesarhu.clojure-euler.euler-149)

(defn sk-vector
  [side]
  (let [m (atom [])
        sk-mod (fn [n] (- (mod n 1000000) 500000))]
    (doseq [k (range 1 56)]
      (let [sk (-> 100003 (- (* 200003 k)) (+ (* 300007 k k k)) sk-mod)]
        (swap! m assoc (dec k) sk)))
    (doseq [k (range 55 (* side side))]
      (let [sk (sk-mod (+ (@m (- k 24)) (@m (- k 55)) 1000000))]
        (swap! m assoc k sk)))
    @m))

(defn max-subseq-sum
  [a [k v]]
  (let [[max-so-far max-ending-here] (if (@a k) (@a k) [0 0])
        here (max 0 (+ max-ending-here v))
        so-far (max here max-so-far)]
    (swap! a merge {k [so-far here]})))

(defn euler-149
  [side]
  (let [v (sk-vector side)
        sum-map (atom {})
        max-sum (atom 0)
        ms (for [i (range side)
                 j (range side)]
             (let [indices [(+ i j 1000000), (+ (- i j) 100000), (+ i 10000), j]
                   k (+ j (* side i))]
               (->> (map #(hash-map % (v k)) indices)
                    (apply merge))))]
    (doseq [m ms
            entry m
            :let [[k v] entry]]
      (max-subseq-sum sum-map entry)
      (reset! max-sum (max @max-sum (first (@sum-map k)))))
    @max-sum))

(comment
  (time (euler-149 2000))
  )
