(ns day04-test
  (:require
    [clojure.java.io :as io]
    [clojure.test :refer [deftest is testing]]
    [day04 :refer [part1 part2]]))


(def my-input
  (delay (slurp (io/resource "input04.txt"))))


(def example
  "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11")


(deftest part1-test
  (testing "example"
    (is (= 13 (part1 example))))
  (testing "my input"
    (is (= 18653 (part1 @my-input)))))


(deftest part2-test
  (testing "example"
    (is (= 30 (part2 example))))
  (testing "my input"
    (is (= 5921508 (part2 @my-input)))))
