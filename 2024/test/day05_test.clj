(ns day05-test
  (:require
    [clojure.java.io :as io]
    [clojure.test :refer [deftest is testing]]
    [day05 :refer [part1 part2]]))

(def my-input
  (delay (slurp (io/resource "input05.txt"))))

(def example1
  "47|53
97|13
97|61
97|47
75|29
61|13
75|53
29|13
97|29
53|29
61|53
97|53
61|29
47|13
75|47
97|75
47|61
75|61
47|29
75|13
53|13

75,47,61,53,29
97,61,53,29,13
75,29,13
75,97,47,61,53
61,13,29
97,13,75,29,47")

(deftest part1-test
  (testing "example"
    (is (= 143 (part1 example1))))
  (testing "my input"
    (is (= 7024 (part1 @my-input)))))

(deftest part2-test
  (testing "example"
    (is (= 123 (part2 example1))))
  (testing "my input"
    (is (= 4151 (part2 @my-input)))))
