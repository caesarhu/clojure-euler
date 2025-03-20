(ns caesarhu.clojure-euler.euler-059)

(def key-length 3)
(def fname "resources/p059_cipher.txt")

(def encrypted-bytes
  (map read-string (clojure.string/split (slurp fname) #",")))

; 假設空白 space 是最多的鍵值
(def space 32)

(defn guess-key
  []
  (for [i (range 3)]
    (let [[k v] (apply max-key val (frequencies (take-nth 3 (drop i encrypted-bytes))))]
      (bit-xor k space))))

(defn euler-059
  []
  (->> (map bit-xor (cycle (guess-key)) encrypted-bytes)
       (apply +)))

(comment
  (time (euler-059))
  )
