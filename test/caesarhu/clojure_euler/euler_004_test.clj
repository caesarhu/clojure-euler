(ns caesarhu.clojure-euler.euler-004-test
  (:require [caesarhu.clojure-euler.euler-004 :refer [euler-004]]
            [clojure.test :refer [deftest is testing]]))

(deftest euler-004-test
  (is (= 906609
         (euler-004))))