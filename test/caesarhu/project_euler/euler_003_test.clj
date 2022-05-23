(ns caesarhu.project-euler.euler-003-test
  (:require [caesarhu.project-euler.euler-003 :refer [euler-003]]
            [clojure.test :refer [deftest is testing]]))

(deftest euler-003-test
  (is (= 6857
         (euler-003 600851475143))))