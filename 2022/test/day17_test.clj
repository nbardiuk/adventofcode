(ns day17-test
  (:require
   [clojure.java.io :as io]
   [clojure.test :refer [deftest is testing]]
   [day17 :refer [part1 part2]]))

(def my-input
  (slurp (io/resource "input17.txt")))

(def example
  ">>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>")

(deftest part1-test
  (testing "example"
    (is (= 3068 (part1 example))))
  (testing "my input"
    (is (= 3085 (part1 my-input)))))

(deftest part2-test
  (testing "example"
    (is (= 1514285714288 (part2 example))))
  (testing "my input"
    (is (= 1535483870924 (part2 my-input)))))
