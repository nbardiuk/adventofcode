(ns day01-test
  (:require
    [clojure.java.io :as io]
    [clojure.test :refer [deftest is testing]]
    [day01 :refer [part1 part2]]))


(def my-input
  (delay (slurp (io/resource "input01.txt"))))


(def example1
  "3   4
4   3
2   5
1   3
3   9
3   3")

(deftest part1-test
  (testing "example"
    (is (= 11 (part1 example1))))
  (testing "my input"
    (is (= 1197984 (part1 @my-input)))))

(deftest part1-test
  (testing "example"
    (is (= 31 (part2 example1))))
  (testing "my input"
    (is (= 23387399 (part2 @my-input)))))


