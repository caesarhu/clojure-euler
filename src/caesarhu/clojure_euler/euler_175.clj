(ns caesarhu.clojure-euler.euler-175
  (:require [caesarhu.math.math-tools :refer [digits]]))

(defn euclid
  "以輾轉相除法(Euclidean algorithm)求得相除過程的商"
  [n m]
  (loop [n n m m
         result []]
    (if (zero? m)
      result
      (recur m (mod n m) (conj result (quot n m))))))

(defn calkin–wilf-tree
  "將商轉換成 Calkin–Wilf tree 序列，如序列個數為偶數，最後1個數淢1，並append 1個1
   https://en.wikipedia.org/wiki/Calkin%E2%80%93Wilf_tree"
  [n m]
  (let [binary (euclid n m)
        append-1 (fn [s]
                   (let [e (last s)]
                     (concat (butlast s) [(dec e) 1])))]
    (if (even? (count binary))
      (append-1 binary)
      binary)))

(defn euclid-reverse
  [n d]
  (loop [n n d d
         result (list)]
    (if (zero? d)
      result
      (recur d (mod n d) (conj result (quot n d))))))

(defn calkin–wilf-reverse
  "將商序列轉換成 Calkin–Wilf tree 序列，如商序列個數為偶數，第1個數淢1，並append 1個1
   https://en.wikipedia.org/wiki/Calkin%E2%80%93Wilf_tree"
  [n d]
  (let [[x & more :as s] (euclid-reverse n d)]
    (if (even? (count s))
      (concat [1 (dec x)] more)
      s)))

(defn solve
  "本題是求f(n)/f(n-1)，是Calkin–Wilf tree序數f(n)/f(n+1)的倒數，故需以倒數代入求解"
  [n d]
  (calkin–wilf-reverse d n))

(defn euler-175
  [n m]
  (clojure.string/join "," (solve n m)))

(comment
  (euler-175 13 17)
  (time (euler-175 123456789 987654321))
  )
