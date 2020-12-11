(ns day11-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [day11 :as sut]))
(def example
  "L.LL.LL.LL
LLLLLLL.LL
L.L.L..L..
LLLL.LL.LL
L.LL.LL.LL
L.LLLLL.LL
..L.L.....
LLLLLLLLLL
L.LLLLLL.L
L.LLLLL.LL")

(def my-input (slurp (io/resource "input11.txt")))

(deftest part1
  (testing "example"
    (is (= 37 (sut/part1 example))))
  (testing "my input"
    (is (= 2368 (sut/part1 my-input)))))

(deftest part2
  (testing "example"
    (is (= 26 (sut/part2 example))))
  (testing "my input"
    (is (= 2124 (sut/part2 my-input)))))
