(ns aoc.day03-test
  (:require [aoc.day03 :refer [part1 part2]]
            [clojure.test :refer [deftest is testing]]))

(def input (slurp "./resources/day03.txt"))

(deftest day3-part1
  (testing "my input"
    (is (= 869 (part1 input)))))
(deftest day3-part2
  (testing "my input"
    (is (= 1544 (part2 input)))))
