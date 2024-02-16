(ns caesarhu.clojure-euler.euler-185
  (:require [caesarhu.kuafu.ortools :refer [or-call]]
            [caesarhu.kuafu.sat :as sat])
  (:import [com.google.ortools.sat LinearExpr]))

(defn ->digits
  ([n] (->digits n 10))
  ([n radix]
   (loop [n n
          res nil]
     (if (zero? n)
       res
       (recur
        (quot n radix)
        (cons (rem n radix) res))))))

(defn ->number
  "Convert a collection of digits to a number"
  ([xs] (->number xs 10))
  ([xs radix]
   (let [expt (fn [x n]
                (apply *' (repeat n x)))
         index (map #(expt radix %) (range))]
     (reduce +' (map *' index (reverse xs))))))

(def base 10)

(defn pos->number
  [x y]
  (+ (* x base) y))

(defn init-model
  [length]
  (let [model (sat/cp-model)
        vars (vec (map #(sat/bool-var model (str "v" %)) (range (* base length))))]
    (doseq [x (range length)
            :let [digits (->> (map #(pos->number x %) (range base))
                              (map vars))]]
      (or-call model addExactlyOne digits))
    {:model model :vars vars}))

(defn rule->constraint
  [m [ds n]]
  (let [{:keys [model vars]} m
        digits (->> (map-indexed vector (->digits ds))
                    (map #(apply pos->number %))
                    (map vars))]
    (or-call model addEquality (or-call LinearExpr sum digits) n)))

(defn rules->model
  [rules]
  (let [length (count (->digits (ffirst rules)))
        m (init-model length)]
    (doseq [rule rules]
      (rule->constraint m rule))
    m))

(defn answer->number
  [answer]
  (->> (map-indexed vector answer)
       (remove #(zero? (last %)))
       (map first)
       (map #(mod % base))
       (->number)))

(defn euler-185
  [rules]
  (let [m (rules->model rules)]
    (->> (sat/solve-all (sat/cp-solver) (:model m) (:vars m))
         :solutions
         first
         answer->number)))

(def target
  [[5616185650518293 2]
   [3847439647293047 1]
   [5855462940810587 3]
   [9742855507068353 3]
   [4296849643607543 3]
   [3174248439465858 1]
   [4513559094146117 2]
   [7890971548908067 3]
   [8157356344118483 1]
   [2615250744386899 2]
   [8690095851526254 3]
   [6375711915077050 1]
   [6913859173121360 1]
   [6442889055042768 2]
   [2321386104303845 0]
   [2326509471271448 2]
   [5251583379644322 2]
   [1748270476758276 3]
   [4895722652190306 1]
   [3041631117224635 3]
   [1841236454324589 3]
   [2659862637316867 2]])

(comment
  (time (euler-185 target))
  )
