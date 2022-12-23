(ns day23-test
  (:require
   [clojure.java.io :as io]
   [clojure.test :refer [deftest is testing]]
   [day23 :refer [part1 part2]]))

(def my-input
  (slurp (io/resource "input23.txt")))
(def example
  "....#..
..###.#
#...#.#
.#...##
#.###..
##.#.##
.#..#..")

(deftest part1-test
  (testing "example"
    (is (= 110 (part1 example))))
  (testing "my input"
    (is (= 3812 (time (part1 my-input))))))

(deftest part2-test
  (testing "example"
    (is (= 20 (part2 example))))
  (testing "my input"
    (is (= 1003 (time (part2 my-input))))))
