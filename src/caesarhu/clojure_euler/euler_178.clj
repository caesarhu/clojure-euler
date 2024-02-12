(ns caesarhu.clojure-euler.euler-178)

(defn digit-steps
  [n]
  (filter #(<= 0 % 9) ((juxt inc dec) n)))

; [maximun minimum current]為3維陣列，代表digit有過的最大值、最小值及目前值
; {[maximun minimum current] n} n為該3維陣列的數量

(def init-digits
  (apply merge (for [i (range 1 10)]
                 {[i i i] 1})))

(defn pandigital?
  [[[maximun minimum current] v]]
  (and (= maximun 9)
       (= minimum 0)))

(defn entry-step
  [[k v :as entry]]
  (let [[maximun minimum current] k
        steps (digit-steps current)]
    (->> (for [step steps]
           {[(max maximun step) (min minimum step) step] v})
         (apply merge-with +'))))

(defn next-step
  [m]
  (->> (map entry-step m)
       (apply merge-with +')))

(defn count-pandigital
  [m]
  (->> (filter pandigital? m)
       (map last)
       (apply +')))

(defn euler-178
  [n]
  (->> (iterate next-step init-digits)
       (take n)
       (map count-pandigital)
       (apply +')))

(comment
  (time (euler-178 40))
  )
