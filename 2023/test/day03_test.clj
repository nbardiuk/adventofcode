(ns day03-test
  (:require
    [clojure.java.io :as io]
    [clojure.test :refer [deftest is testing]]
    [day03 :refer [part1 part2]]))


(def my-input
  (delay (slurp (io/resource "input03.txt"))))


(def example
  "467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598..")


(deftest part1-test
  (testing "example"
    (is (= 4361 (part1 example))))
  (testing "my input"
    (is (= 532428 (part1 @my-input)))))


(deftest part2-test
  (testing "example"
    (is (= 467835 (part2 example))))
  (testing "my input"
    (is (= 84051670 (part2 @my-input)))))
