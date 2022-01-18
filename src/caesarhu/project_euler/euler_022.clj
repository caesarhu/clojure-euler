(ns caesarhu.project-euler.euler-022)

(def euler-022-file "resources/p022_names.txt")

(defn get-data 
  []
  (sort (clojure.string/split (slurp euler-022-file) #",")))

(defn name-score
  [names]
  (let [score (for [name names]
                (->> (seq name)
                     (map int)
                     (map #(- % 64))
                     (remove neg?)
                     (apply +)))
        counter (rest (range))]
    (->> (map #(* %1 %2) score counter)
         (apply +))))

(defn euler-022
  []
  (name-score (get-data)))

(comment
  (time (euler-022))
  )