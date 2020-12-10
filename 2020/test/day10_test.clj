(ns day10-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [day10 :as sut]))
(def example1
  "16
10
15
5
1
11
7
19
6
12
4")
(def example2
  "28
33
18
42
31
14
46
20
48
47
24
23
49
45
19
38
39
11
1
32
25
35
8
17
7
9
4
2
34
10
3")

(def my-input (slurp (io/resource "input10.txt")))

(deftest part1
  (testing "example1"
    (is (= 35 (sut/part1 example1))))
  (testing "example2"
    (is (= 220 (sut/part1 example2))))
  (testing "my input"
    (is (= 2346 (sut/part1 my-input)))))

(deftest part2
  (testing "example1"
    (is (= 8 (sut/part2 example1))))
  (testing "example1"
    (is (= 19208 (sut/part2 example2))))
  (testing "my input"
    (is (= 6044831973376 (sut/part2 my-input)))))
