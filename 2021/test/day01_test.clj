(ns day01-test
  (:require
   [clojure.test :refer [deftest testing is]]
   [day01 :refer [part1]]))

(deftest part1-test
  (testing "example"
    (is (= 1 (part1 "asdf")))))
