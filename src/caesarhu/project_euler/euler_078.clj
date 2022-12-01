(ns caesarhu.project-euler.euler-078)

(def mod-number 1000000)
(def sign (cycle [1 -1]))

(defn pentagon
  [n]
  (quot (- (* n n 3) n) 2))

(def pentagon-algorithm
  (memoize
   (fn [n]
     (cond
       (= n 0) 1
       (neg? n) 0
       :else (letfn [(term [k] (+ (pentagon-algorithm (- n (pentagon k)))
                                  (pentagon-algorithm (- n (pentagon (- k))))))]
               (mod
                (reduce + (take-while (comp not zero?)
                                      (map #(* (term %2) %1) sign (iterate inc 1))))
                mod-number))))))

(defn euler-078
  []
  (->> (iterate inc 1)
       (some #(and (zero? (pentagon-algorithm %)) %))))

(comment
  (time (euler-078))
  )