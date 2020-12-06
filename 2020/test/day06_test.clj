(ns day06-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [day06 :as sut]))

(def example
  "abc

a
b
c

ab
ac

a
a
a
a

b")

(def my-input (slurp (io/resource "input6.txt")))

(deftest part1
  (testing "example"
    (is (= 11 (sut/part1 example))))
  (testing "my input"
    (is (= 6885 (sut/part1 my-input)))))

(deftest part2
  (testing "example"
    (is (= 6 (sut/part2 example))))
  (testing "my input"
    (is (= 3550 (sut/part2 my-input)))))
