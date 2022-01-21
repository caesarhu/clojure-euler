(ns caesarhu.project-euler.euler-059)

(def fname "resources/p059_cipher.txt")

(defn get-encrypted-bytes [fname]
  (map read-string (clojure.string/split (slurp fname) #",")))

(defn char->int [c]
  (int c))

(defn int->char
  [n]
  (char n))

(defn build-score-map [] ;; See https://en.wikipedia.org/wiki/Etaoin_shrdlu
  (let [freq-phrase (apply str (reverse "ETAOIN SHRDLU"))
        upper-freqs (map vector freq-phrase (rest (range)))
        lower-freqs (map vector (.toLowerCase freq-phrase) (rest (range)))]
    (into {} (concat upper-freqs lower-freqs))))

(defn bytes-score
  [score-map bytes mask]
  (->> (map bit-xor bytes (cycle mask))
       (map (comp score-map int->char))
       (remove nil?)
       (reduce +)))

(defn euler-059 []
  (let [input (get-encrypted-bytes fname)
        score-map  (build-score-map)
        char-range (range (char->int \a) (inc (char->int \z)))
        masks      (for [a char-range b char-range c char-range] [a b c])
        mask       (apply max-key (partial bytes-score score-map input) masks)
        result (map bit-xor input (cycle mask))]
    {:sum (apply + result)}
    (->> (map bit-xor input (cycle mask))
         ;(map int->char)
         (apply +))))

(comment
  (time (euler-059))
  )