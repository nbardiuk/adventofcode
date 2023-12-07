(ns day07-test
  (:require
    [clojure.java.io :as io]
    [clojure.test :refer [deftest is testing]]
    [day07 :refer [part1 part2]]))


(def my-input
  (delay (slurp (io/resource "input07.txt"))))


(def example
  "32T3K 765
T55J5 684
KK677 28
KTJJT 220
QQQJA 483")


(deftest part1-test
  (testing "example"
    (is (= 6440 (part1 example))))
  (testing "my input"
    (is (= 249726565 (part1 @my-input)))))


(deftest part2-test
  (testing "example"
    (is (= 5905 (part2 example))))
  (testing "my input"
    (is (= 251135960 (part2 @my-input)))))
