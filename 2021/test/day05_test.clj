(ns day05-test
  (:require
   [clojure.java.io :as io]
   [clojure.test :refer [deftest is testing]]
   [day05 :refer [part1 part2]]))

(def example
  "0,9 -> 5,9
8,0 -> 0,8
9,4 -> 3,4
2,2 -> 2,1
7,0 -> 7,4
6,4 -> 2,0
0,9 -> 2,9
3,4 -> 1,4
0,0 -> 8,8
5,5 -> 8,2")

(def my-input (slurp (io/resource "input5.txt")))

(deftest part1-test
  (testing "example"
    (is (= 5 (part1 example))))
  (testing "my input"
    (is (= 6007 (part1 my-input)))))

(deftest part2-test
  (testing "example"
    (is (= 12 (part2 example))))
  (testing "my input"
    (is (= 19349 (part2 my-input)))))
