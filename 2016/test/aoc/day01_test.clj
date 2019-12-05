(ns aoc.day01-test
  (:require [clojure.test :refer [deftest testing is]]
            [aoc.day01 :refer [part1]]))

(def input (slurp "./resources/day01.txt"))

(deftest day1-part1
  (testing "examples"
    (is (= (part1 "R2, L3") 5)) 
    (is (= (part1 "R2, R2, R2") 2))
    (is (= (part1 "R5, L5, R5, R3") 12)))
  (testing "my input"
    (is (= (part1 input) 241))))
