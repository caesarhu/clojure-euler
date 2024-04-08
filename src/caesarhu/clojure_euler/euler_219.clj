(ns caesarhu.clojure-euler.euler-219)

(def compendium {1 1, 2 0, 3 0, 4 1})

(defn next-compendium
  [compendium]
  (let [small (apply min (keys compendium))]
    (-> (merge-with + compendium {(inc small) (compendium small)})
        (assoc (+ small 4) (compendium small))
        (dissoc small))))

(defn compendium-pair
  [target]
  (loop [compendium compendium
         compendium-prev nil]
    (if (< (apply + (vals compendium)) target)
      (recur (next-compendium compendium) compendium)
      [compendium compendium-prev])))

(defn keys-sum
  [m]
  (->> (for [k (keys m)]
         (* k (m k)))
       (apply +)))

(defn euler-219
  [target]
  (let [[compendium compendium-prev] (compendium-pair target)
        [a b] (map #(apply + (vals %)) [compendium compendium-prev])
        [s t] (map keys-sum [compendium compendium-prev])
        u (quot (- s t) (- a b))
        d (- a target)]
    (- s (* d u))))

(comment
  (time (euler-219 1000000000))
  )
