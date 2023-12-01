(ns day01-test
  (:require
    [clojure.java.io :as io]
    [clojure.test :refer [deftest is testing]]
    [day01 :refer [part1 part2]]))


(def my-input
  (delay (slurp (io/resource "input01.txt"))))


(def example1
  "1abc2
pqr3stu8vwx
a1b2c3d4e5f
treb7uchet")


(def example2
  "two1nine
eightwothree
abcone2threexyz
xtwone3four
4nineeightseven2
zoneight234
7pqrstsixteen")


(deftest part1-test
  (testing "example"
    (is (= 142 (part1 example1))))
  (testing "my input"
    (is (= 53386 (part1 @my-input)))))


(deftest part2-test
  (testing "example"
    (is (= 281 (part2 example2))))
  (testing "my input"
    (is (= 53312 (part2 @my-input)))))
