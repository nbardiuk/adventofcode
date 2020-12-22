(ns day22-test
  (:require [clojure.test :refer (is testing deftest)]
            [clojure.java.io :as io]
            [day22 :as sut]))

(def example
  "Player 1:
9
2
6
3
1

Player 2:
5
8
4
7
10")

(def my-input (slurp (io/resource "input22.txt")))

(deftest part1
  (testing "example"
    (is (= 306 (sut/part1 example))))
  (testing "my input"
    (is (= 31455 (sut/part1 my-input)))))

(deftest part2
  (testing "example"
    (is (= 291 (sut/part2 example))))
  (testing "my input"
    (is (= 32528 (sut/part2 my-input)))))
