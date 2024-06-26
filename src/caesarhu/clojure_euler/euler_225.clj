(ns caesarhu.clojure-euler.euler-225)

(defn non-divisors-loop
  [^long n]
  (loop [v [1 1 1]]
    (let [new (mod (apply + v) n)
          new-v (conj (subvec v 1) new)]
      (cond
        (zero? new) false
        (= new-v [1 1 1]) n
        :else (recur new-v)))))

(defn euler-225
  [^long target]
  (nth (->> (iterate (partial + 2) 3)
            (filter non-divisors-loop))
       (dec target)))

(comment
  (time (euler-225 124))
  )
