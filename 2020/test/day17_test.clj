(ns day17-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [day17 :as sut]))

(def example
  ".#.
..#
###")

(def my-input (slurp (io/resource "input17.txt")))

(deftest part1
  (testing "example"
    (is (= 112 (sut/part1 example))))
  (testing "my input"
    (is (= 306 (sut/part1 my-input)))))

(deftest part2
  (testing "example"
    (is (= 848 (sut/part2 example))))
  (testing "my input"
    (is (= 2572 (sut/part2 my-input)))))
