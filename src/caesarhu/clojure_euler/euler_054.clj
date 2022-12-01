(ns caesarhu.clojure-euler.euler-054
  (:require [caesarhu.math.math-tools :as tools]))

(def card-nums (zipmap "23456789TJQKA" (iterate inc 2)))

(def card-suits (set "HDSC"))

(defn card-val [c] (card-nums (first c)))

(def hand-rank (zipmap [:straight-flush
                        :four-of-a-kind
                        :full-house
                        :flush
                        :straight
                        :three-of-a-kind
                        :two-pairs
                        :pair
                        :high-card] (range)))

(defn hand
  "Returns the type of the hand and a list of the card groups."
  [cards]
  (let [cs (sort #(> (card-val %1) (card-val  %2)) cards)
        ns (into (sorted-set) (map #(card-val %) cs))
        straight? (and (= 5 (count ns)) (= 4 (- (last ns) (first ns))))
        flush? (apply = (map second cards))
        groups (sort #(> (count %1) (count %2))
                     (vals (group-by #(card-val %) cs)))
        g1 (count (first groups))
        g2 (count (second groups))
        hand-type (cond
                    (and straight? flush?)  :straight-flush
                    (= 4 g1)                :four-of-a-kind
                    (and (= 3 g1) (= 2 g2)) :full-house
                    flush?                  :flush
                    straight?               :straight
                    (= 3 g1)                :three-of-a-kind
                    (and (= 2 g1) (= 2 g2)) :two-pairs
                    (= 2 g1)                :pair
                    :else                   :high-card)]
    [hand-type groups]))

(defn break-tie
  "Assuming that g1 and g2 represent the same type of hand (eg: full house),
  returns the outcome for the g1 hand (:win, :lose, or rarely :tie)"
  [g1 g2]
  (let [a (first (first g1))
        b (first (first g2))]
    (if (or (nil? a) (nil? b))
      :tie
      (cond
        (> (card-val a) (card-val b)) :win
        (< (card-val a) (card-val b)) :lose
        :else (recur (next g1) (next g2))))))

(defn i-win? [me you]
  (let [[tm gm] (hand me)
        [ty gy] (hand you)]
    (cond
      (< (hand-rank tm) (hand-rank ty)) :win
      (> (hand-rank tm) (hand-rank ty)) :lose
      :else (break-tie gm gy))))

(defn euler-054 [file]
  (count
   (for [line (clojure.string/split-lines (slurp file))
         [me you] [(split-at 5 (re-seq #"\S+" line))]
         :when (= (i-win? me you) :win)]
     :win)))

(comment
  (euler-054 "resources/p054_poker.txt")
  )
