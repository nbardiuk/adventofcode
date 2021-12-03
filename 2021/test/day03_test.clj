(ns day03-test
  (:require
   [clojure.java.io :as io]
   [clojure.test :refer [deftest is testing]]
   [day03 :refer [part1 part2]]))

(def example
  "00100
11110
10110
10111
10101
01111
00111
11100
10000
11001
00010
01010")

(def my-input (slurp (io/resource "input3.txt")))

(deftest part1-test
  (testing "example"
    (is (= 198 (part1 example))))
  (testing "my input"
    (is (= 3969000 (part1 my-input)))))

(deftest part2-test
  (testing "example"
    (is (= 230 (part2 example))))
  (testing "my input"
    (is (= 4267809 (part2 my-input)))))
