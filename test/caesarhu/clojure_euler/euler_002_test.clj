(ns caesarhu.clojure-euler.euler-002-test
  (:require [caesarhu.clojure-euler.euler-002 :refer [euler-002]]
            [clojure.test :refer [deftest is testing]]))

(deftest euler-002-test
  (is (= 4613732
         (euler-002 4000000))))