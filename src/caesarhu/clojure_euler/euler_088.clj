(ns caesarhu.clojure-euler.euler-088)

(defn product-loop
  [limit result-map]
  (letfn [(product-sum [product sum c start] ; p:product s:sum c:count of factors
            (let [k (+ product (- sum) c)]
              (when (<= k limit)
                (swap! result-map #(merge-with min %1 %2) {k product})
                (doseq [i (iterate inc start)
                        :let [next-product (* product i)]
                        :while (<= next-product (* 2 limit))]
                  (product-sum next-product (+ sum i) (inc c) i)))))]
    (product-sum 1 1 1 2)))

(defn euler-088-atom
  [limit]
  (let [result-map (atom {})]
    (product-loop limit result-map)
    (->> (dissoc @result-map 1)
         vals
         set
         (apply +))))

(defn euler-088
  [limit]
  (letfn [(map-loop
           [m p s c start]
           (let [k (+ p (- s) c)]
             (if (> k limit) m
               (apply merge-with min m {k p}
                      (for [i (iterate inc start)
                            :let [next-product (* p i)]
                            :while (<= next-product (* 2 limit))]
                        (map-loop m next-product (+ s i) (inc c) i))))))]
    (apply + (-> (map-loop {} 1 1 1 2)
                 (dissoc 1)
                 vals
                 set))))

(comment
  (time (euler-088 12000))
  (time (euler-088-atom 12000))
  )