(ns caesarhu.clojure-euler.euler-088)

(defn euler-088-atom
  [limit]
  (let [result-map (atom {})]
    (letfn [(map-loop [product sum count start]
              (let [k (+ product (- sum) count)]
                (when (<= k limit)
                  (swap! result-map #(merge-with min %1 %2) {k product})
                  (doseq [i (iterate inc start)
                          :let [next-product (* product i)]
                          :while (<= next-product (* 2 limit))]
                    (map-loop next-product (+ sum i) (inc count) i)))))]
      (map-loop 1 1 1 2)
      (->> (dissoc @result-map 1) vals set (apply +)))))

(defn euler-088
  [limit]
  (letfn [(map-loop [product sum count start]
            (let [k (+ product (- sum) count)]
              (when (<= k limit)
                (apply merge-with min {k product}
                       (for [i (iterate inc start)
                             :while (<= (* product i) (* 2 limit))]
                         (map-loop (* product i) (+ sum i) (inc count) i))))))]
    (apply + (-> (map-loop 1 1 1 2) (dissoc 1) vals set))))

(comment
  (time (euler-088 12000))
  (time (euler-088-atom 12000))
  )