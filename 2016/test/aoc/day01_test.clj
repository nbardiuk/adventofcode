(ns aoc.day01-test
  (:require [aoc.day01 :refer [part1 part2]]
            [clojure.test :refer [deftest is testing]]))

(def input (slurp "./resources/day01.txt"))

(deftest day1-part1
  (testing "examples"
    (is (= (part1 "R2, L3") 5))
    (is (= (part1 "R2, R2, R2") 2))
    (is (= (part1 "R5, L5, R5, R3") 12)))
  (testing "my input"
    (is (= (part1 input) 241))))

(deftest day1-part2
  (testing "example"
    (is (= (part2 "R8, R4, R4, R8") 4)))
  (testing "my input"
    (is (= (part2 input) 116))))
