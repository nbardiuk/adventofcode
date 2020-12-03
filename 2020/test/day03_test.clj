(ns day03-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [day03 :as sut]))

(def example
  "..##.......
#...#...#..
.#....#..#.
..#.#...#.#
.#...##..#.
..#.##.....
.#.#.#....#
.#........#
#.##...#...
#...##....#
.#..#...#.#")

(def my-input (slurp (io/resource "input3.txt")))

(deftest part1
  (testing "example"
    (is (= 7 (sut/part1 example))))
  (testing "my input"
    (is (= 218 (sut/part1 my-input)))))

(deftest part2
  (testing "example"
    (is (= 336 (sut/part2 example))))
  (testing "my input"
    (is (= 3847183340 (sut/part2 my-input)))))
