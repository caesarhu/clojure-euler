(ns caesarhu.project-euler.euler-037
  (:require [caesarhu.math.math-tools :as tools]
            [caesarhu.math.primes :as p]))

(defn truncatable-prime?
  [n]
  (let [ds (tools/digits n)
        truncate (fn [ds]
                   (loop [ds ds
                          dsr ds
                          result []]
                     (if (= (count ds) 1)
                       result
                       (let [new-ds (rest ds)
                             new-dsr (butlast dsr)]
                         (recur new-ds new-dsr (conj result 
                                                     (tools/digits->number new-ds)
                                                     (tools/digits->number new-dsr)))))))]
    (->> (cons n (truncate ds))
         (every? p/is-prime?))))

(defn brute-force
  []
  (->> (drop 4 p/primes)
       (filter truncatable-prime?)
       (take 11)
       (apply +)))

(comment
  (time (brute-force))
  )