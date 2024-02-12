(ns caesarhu.clojure-euler.euler-177)

(def right-angle 90)
(def tri-sum 180)
(def tolerance (apply * (repeat 9 10)))

(defn tolerant-int
  [x]
  (long (+ (* x tolerance) 0.5)))

(defn sin
  [degree]
  (Math/sin (Math/toRadians degree)))

(defn logsin
  [n]
  (Math/log (sin n)))

(def logsin-map
  (->> (map #(vector % (logsin %)) (range 1 (inc tri-sum)))
       (cons [0 0])
       (into {})))

(defn flip
  [[a b]]
  [b a])

(defn deep-flip
  [[s1 s2]]
  [(flip s1) (flip s2)])

(defn reflect
  [[angle4-1 angle4-2]]
  [(deep-flip (flip angle4-2)) (deep-flip (flip angle4-1))])

(defn rotate
  [[angle4-1 angle4-2]]
  [[(last angle4-1) (first angle4-2)] [(last angle4-2) (first angle4-1)]])

(defn canonise
  [angle8]
  (reduce #(if (neg-int? (compare %2 %1)) %2 %1)
          (reductions #(%2 %1) angle8 [rotate rotate rotate reflect rotate rotate rotate])))

(defn generate-log-ratios
  [theta]
  (let [psi (- tri-sum theta)]
    (->> (for [alpha (range 1 theta)
               beta (range 1 psi)
               :let [xr (+ (logsin-map alpha) (logsin-map beta)
                           (- (logsin-map (- theta alpha))) (- (logsin-map (- psi beta))))
                     xr2 (tolerant-int (+ xr 10))]]
           {xr2 [[[alpha (- theta alpha)] [beta (- psi beta)]]]})
         (apply merge-with concat))))

(defn count-angles
  []
  (->> (for [theta (range 2 (inc right-angle))]
         (->> (for [[_ v] (generate-log-ratios theta)
                    angle4-1 v
                    angle4-2 v]
                {(canonise [angle4-1 (deep-flip angle4-2)]) 1})
              (apply merge-with +)))
       (apply merge-with +)))

(defn euler-177
  []
  (count (count-angles)))

(comment
  (time (euler-177))
  )
