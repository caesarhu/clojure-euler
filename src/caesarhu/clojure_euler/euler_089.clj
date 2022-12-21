(ns caesarhu.clojure-euler.euler-089)

(def r2n (zipmap "IVXLCDM" [1 5 10 50 100 500 1000]))

(def n2r (zipmap (vals r2n) (keys r2n)))

;; Do things in reverse and it's so much easier to solve!
(defn de-romanize
  "Returns the decimal representation of a roman numeral string s. "
  ([s] (de-romanize (reverse (map r2n s)) 0 0))
  ([s total mx] (if-let [c (first s)]
                  (if (>= c mx)
                    (recur (rest s) (+ total c) (max c mx))
                    (recur (rest s) (- total c) mx))
                  total)))

(defn romanize
  "Returns the minimal roman numeral representation of n"
  ([n]
   {:pre [(<= n 10000)]}
   (romanize (quot n 10) (rem n 10) 1))
  ([q r x]
   (if (> x 100)
     (repeat (+ q r) (n2r x))
     (->> (concat
           (romanize (quot q 10) (rem q 10) (* x 10))
           (cond
             (< r 4) (repeat r (n2r x))
             (= r 4) [(n2r x) (n2r (* 5 x))]
             (< r 9) (concat [(n2r (* 5 x))] (repeat (- r 5) (n2r x)))
             (= r 9) [(n2r x) (n2r (* 10 x))]
             :else ""))
          (apply str)))))

(defn euler-89
  [file]
  (reduce + (for [l (clojure.string/split-lines (slurp file))]
              (- (count l)
                 (count (romanize (de-romanize l)))))))

(comment
  (time (euler-89 "resources/p089_roman.txt"))
  )