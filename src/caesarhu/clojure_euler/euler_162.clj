(ns caesarhu.clojure-euler.euler-162)

(def hex-base 16)

(defn euler-162
  [hex-digits]
  (loop [hex-digits hex-digits
         [n a b c d] [0 0 0 0 1]]
    (if (pos-int? hex-digits)
      (recur (dec hex-digits) [(+' n (*' (- hex-base 3) a) (*' 2 b))
                               (+' (*' (- hex-base 3) a) (*' 3 b))
                               (+' (*' (- hex-base 2) b) (*' 2 c))
                               (+' (*' (dec hex-base) c) d)
                               (*' hex-base d)])
      (format "%X" n))))

(comment
  (time (euler-162 16))
  )
