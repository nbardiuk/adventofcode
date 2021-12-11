(ns day11-test
  (:require
   [clojure.java.io :as io]
   [clojure.test :refer [deftest is testing]]
   [day11 :refer [part1 part2]]))

(def example
  "5483143223
2745854711
5264556173
6141336146
6357385478
4167524645
2176841721
6882881134
4846848554
5283751526")

(def my-input (slurp (io/resource "input11.txt")))

(deftest part1-test
  (testing "example"
    (is (= 1656 (part1 example))))
  (testing "my input"
    (is (= 1585 (part1 my-input)))))

(deftest part2-test
  (testing "example"
    (is (= 195 (part2 example))))
  (testing "my input"
    (is (= 382 (part2 my-input)))))
