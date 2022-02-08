(ns caesarhu.project-euler.euler-079
  (:require [caesarhu.math.math-tools :as tools]))

(def keylog
  (->> (slurp "resources/p079_keylog.txt")
       (clojure.string/split-lines)
       (map #(Long/parseLong %))
       (map tools/digits)))