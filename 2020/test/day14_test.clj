(ns day14-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [day14 :as sut]))

(def example1
  "mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X
mem[8] = 11
mem[7] = 101
mem[8] = 0")

(def example2
  "mask = 000000000000000000000000000000X1001X
mem[42] = 100
mask = 00000000000000000000000000000000X0XX
mem[26] = 1")

(def my-input (slurp (io/resource "input14.txt")))

(deftest part1
  (testing "example"
    (is (= 165 (sut/part1 example1))))
  (testing "my input"
    (is (= 15172047086292 (sut/part1 my-input)))))

(deftest part2
  (testing "example"
    (is (= 208 (sut/part2 example2))))
  (testing "my input"
    (is (= 4197941339968 (sut/part2 my-input)))))
