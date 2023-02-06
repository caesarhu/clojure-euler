(ns user
  (:require [portal.api :as p]
            [clojure.java.shell :as sh]
            [clojure.string :as str]))

(comment
  (def p (p/open))
  (add-tap #'p/submit)
  (tap> :hello)
  (sh/sh "hostname" "-f")
  )