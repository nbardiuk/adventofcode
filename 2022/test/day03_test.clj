(ns day03-test
  (:require
   [clojure.java.io :as io]
   [clojure.test :refer [deftest is testing]]
   [day03 :refer [part1 part2]]))

(def my-input
  (slurp (io/resource "input03.txt")))

(def example
  "vJrwpWtwJgWrhcsFMMfFFhFp
jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
PmmdzqPrVvPwwTWBwg
wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
ttgJtRGJQctTZtZT
CrZsJsPPZsGzwwsLwLmpwMDw")

(deftest part1-test
  (testing "example"
    (is (= 157 (part1 example))))
  (testing "my input"
    (is (= 8202 (part1 my-input)))))

(deftest part2-test
  (testing "example"
    (is (= 70 (part2 example))))
  (testing "my input"
    (is (= 2864(part2 my-input)))))
