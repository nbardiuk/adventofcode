(ns aoc.day02-test
  (:require [aoc.day02 :refer [part1 part2]]
            [clojure.test :refer [deftest is testing]]))

(def input (slurp "./resources/day02.txt"))

(deftest day2-part1
  (testing "example"
    (is (= (part1 "ULL\nRRDDD\nLURDL\nUUUUD\n") "1985")))
  (testing "my input"
    (is (= (part1 input) "92435"))))

(deftest day2-part2
  (testing "example"
    (is (= (part2 "ULL\nRRDDD\nLURDL\nUUUUD\n") "5DB3")))
  (testing "my input"
    (is (= (part2 input) "C1A88"))))
