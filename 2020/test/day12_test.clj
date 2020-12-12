(ns day12-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [day12 :as sut]))

(def example
  "F10
N3
F7
R90
F11")

(def my-input (slurp (io/resource "input12.txt")))

(deftest part1
  (testing "example"
    (is (= 25 (sut/part1 example))))
  (testing "my input"
    (is (= 962 (sut/part1 my-input)))))

(deftest part2
  (testing "example"
    (is (= 286 (sut/part2 example))))
  (testing "my input"
    (is (= 56135 (sut/part2 my-input)))))
