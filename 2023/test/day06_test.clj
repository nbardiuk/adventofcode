(ns day06-test
  (:require
    [clojure.java.io :as io]
    [clojure.test :refer [deftest is testing]]
    [day06 :refer [part1 part2]]))


(def my-input
  (delay (slurp (io/resource "input06.txt"))))


(def example
  "Time:      7  15   30
Distance:  9  40  200")


(deftest part1-test
  (testing "example"
    (is (= 288 (part1 example))))
  (testing "my input"
    (is (= 588588 (part1 @my-input)))))


(deftest part2-test
  (testing "example"
    (is (= 71503 (part2 example))))
  (testing "my input"
    (is (= 34655848 (part2 @my-input)))))
