(ns caesarhu.project-euler.euler-019)

(def days
  (cycle (list :mon :tue :wed :thu :fri :sat :sun)))

(def months
  (cycle (list :jan :feb :mar :apr :may :jun :jul :aug :sep :oct :nov :dec)))

(defn leap-year? [y]
  (and
   (zero? (mod y 4))
   (or
    (pos? (mod y 100))
    (zero? (mod y 400)))))

(defn days-in-month
  [m y]
  (let [num-days {:jan 31, :feb (if (leap-year? y) 29 28) , :mar 31
                  :apr 30, :may 31, :jun 30
                  :jul 31, :aug 31, :sep 30
                  :oct 31, :nov 30, :dec 31}]
    (m num-days)))

(defn next-month
  [m]
  (fnext (drop-while #(not (= % m)) months)))

(defn dates-from
  [d m y]
  (lazy-seq
   (let [rollover-month? (>= d (days-in-month m y))
         rollover-year? (and rollover-month? (= m :dec))
         new-day   (if rollover-month? 1 (inc d))
         new-month (if rollover-month? (next-month m) m)
         new-year  (if rollover-year?  (inc y) y)]
     (cons [d m y] (dates-from new-day new-month new-year)))))

(defn entry-criteria 
  [[day [d m y]]]
  (not
   (and
    (= d 1)
    (= m :jan)
    (= y 1901))))

(defn exit-criteria 
  [[day [d m y]]]
  (not
   (and
    (= d 1)
    (= m :jan)
    (= y 2001))))

(defn first-sunday 
  [[day [d m y]]]
  (and
   (= day :sun)
   (= d 1)))

(defn euler-019 
  []
  (->> (map vector days (dates-from 1 :jan 1900))
       (drop-while entry-criteria)
       (take-while exit-criteria)
       (filter first-sunday)
       count))

(comment
  (time (euler-019))
  )