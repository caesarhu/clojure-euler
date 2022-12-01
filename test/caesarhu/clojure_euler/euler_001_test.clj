(ns caesarhu.clojure-euler.euler-001-test
  (:require [caesarhu.clojure-euler.euler-001 :refer [euler-001]]
            [clojure.test :refer [deftest is testing]]))

(deftest euler-001-test
  (is (= 233168
         (euler-001 1000))))