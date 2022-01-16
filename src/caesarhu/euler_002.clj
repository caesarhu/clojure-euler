(ns caesarhu.euler-002)

(def fib-seq
  ((fn fib [a b]
     (lazy-seq (cons a (fib b (+ a b)))))
   0 1))

(defn euler-002
  [limit]
  (->> (take-while #(< % limit) fib-seq)
       (filter even?)
       (apply +)))

(comment
  (time (euler-002 4000000))
  )