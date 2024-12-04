(ns day04-test
  (:require
    [clojure.java.io :as io]
    [clojure.test :refer [deftest is testing]]
    [day04 :refer [part1 part2]]))

(def my-input
  (delay (slurp (io/resource "input04.txt"))))

(def example1
  "MMMSXXMASM
MSAMXMSMSA
AMXSXMAAMM
MSAMASMSMX
XMASAMXAMM
XXAMMXXAMA
SMSMSASXSS
SAXAMASAAA
MAMMMXMMMM
MXMXAXMASX")

(deftest part1-test
  (testing "example"
    (is (= 18 (part1 example1))))
  (testing "my input"
    (is (= 2549 (part1 @my-input)))))

(deftest part2-test
  (testing "example"
    (is (= 9 (part2 example1))))
  (testing "my input"
    (is (= 2003 (part2 @my-input)))))
