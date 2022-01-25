(ns caesarhu.project-euler.euler-061)

(defn polygon
  ([a b q]
   (fn [n]
     (quot (+ (* a n n) (* b n)) q)))
  ([a b]
   (polygon a b 1)))

(def polygon-fns
  (map #(apply polygon %)
       [[1 1 2]  ; p3
        [1 0]    ; p4
        [3 -1 2] ; p5
        [2 -1]   ; p6
        [5 -3 2] ; p7
        [3 -2]])) ; p8

(defn seperate
  [n]
  [(quot n 100) (mod n 100)])

(defn take-digits
  [f]
  (->> (map f (iterate inc 1))
       (drop-while #(< % 1000))
       (take-while #(< % 10000))
       (map (fn [n]
              (let [[x y] (seperate n)]
                {x [y]})))
       (apply merge-with concat)))

(defn generate-polygon-map
  []
  (->> (for [i (range (count polygon-fns))]
         {i (take-digits (nth polygon-fns i))})
       (apply merge)))

(def polygon-map (generate-polygon-map))

(defn next-f
  [xs]
  (let [fs (map first xs)
        m (apply dissoc polygon-map fs)
        head (-> xs last last)
        tails ((polygon-map (-> xs last first)) head)]
    (if (empty? m)
      (when (some #(= (-> xs first last) %) tails)
        xs)
      (for [tail tails
            k (keys m)
            :let [thead ((m k) tail)]
            :when thead]
        (conj xs [k tail])))))

(defn euler-061
  []
  (loop [i 6
         result (->> (keys (polygon-map 5))
                     (map #(vector [5 %])))]
    (if (zero? i)
      (let [cycle (map last result)]
        (->> (partition 2 1 (cons (last cycle) cycle))
             (map #(+ (* (first %) 100) (last %)))
             (apply +)))
      (recur (dec i) (mapcat next-f result)))))

(comment
  (time (euler-061))
  )