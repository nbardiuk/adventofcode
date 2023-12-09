(ns day09-test
  (:require
    [clojure.java.io :as io]
    [clojure.test :refer [deftest is testing]]
    [day09 :refer [part1 part2]]))


(def my-input
  (delay (slurp (io/resource "input09.txt"))))


(def example
  "0 3 6 9 12 15
1 3 6 10 15 21
10 13 16 21 30 45")


(deftest part1-test
  (testing "example"
    (is (= 114 (part1 example))))
  (testing "my input"
    (is (= 1819125966 (part1 @my-input)))))


(deftest part2-test
  (testing "example"
    (is (= 2 (part2 example))))
  (testing "my input"
    (is (= 1140 (part2 @my-input)))))
