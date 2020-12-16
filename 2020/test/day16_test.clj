(ns day16-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [day16 :as sut]))

(def example
  "class: 1-3 or 5-7
row: 6-11 or 33-44
seat: 13-40 or 45-50

your ticket:
7,1,14

nearby tickets:
7,3,47
40,4,50
55,2,20
38,6,12")

(def my-input (slurp (io/resource "input16.txt")))

(deftest part1
  (testing "example"
    (is (= 71 (sut/part1 example))))
  (testing "my input"
    (is (= 27911 (sut/part1 my-input)))))

(deftest part2
  (testing "my input"
    (is (= 737176602479 (sut/part2 my-input)))))
