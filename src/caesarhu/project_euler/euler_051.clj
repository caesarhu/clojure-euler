(ns caesarhu.project-euler.euler-051
  (:require [caesarhu.math.math-tools :as tools]
            [caesarhu.math.primes :as p]))

(defn unique-digits
  [n]
  (-> (tools/digits n) distinct))

(defn replace-digit
  [n d]
  (->> (for [i (range 10)]
         (for [j (tools/digits n)]
           (if (= j d) i j)))
       (filter #(pos-int? (first %)))
       (map tools/digits->number)))

(def prime-set (set (p/primes-range 1000000)))

(defn count-primes
  [s]
  (->> (filter prime-set s)
       count))

(defn is-target?
  [t n]
  (->> (map #(replace-digit n %) (unique-digits n))
       (some #(>= (count-primes %) t))))

(defn euler-051
  [n]
  (some #(and (is-target? n %) %) p/primes))

(comment
  (time (euler-051 8))
  )