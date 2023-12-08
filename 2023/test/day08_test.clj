(ns day08-test
  (:require
    [clojure.java.io :as io]
    [clojure.test :refer [deftest is testing]]
    [day08 :refer [part1 part2]]))


(def my-input
  (delay (slurp (io/resource "input08.txt"))))


(def example1
  "RL

AAA = (BBB, CCC)
BBB = (DDD, EEE)
CCC = (ZZZ, GGG)
DDD = (DDD, DDD)
EEE = (EEE, EEE)
GGG = (GGG, GGG)
ZZZ = (ZZZ, ZZZ)")


(def example2
  "LLR

AAA = (BBB, BBB)
BBB = (AAA, ZZZ)
ZZZ = (ZZZ, ZZZ)")


(def example3
  "LR

11A = (11B, XXX)
11B = (XXX, 11Z)
11Z = (11B, XXX)
22A = (22B, XXX)
22B = (22C, 22C)
22C = (22Z, 22Z)
22Z = (22B, 22B)
XXX = (XXX, XXX)")


(deftest part1-test
  (testing "examples"
    (is (= 2 (part1 example1)))
    (is (= 6 (part1 example2))))
  (testing "my input"
    (is (= 19099 (part1 @my-input)))))


(deftest part2-test
  (testing "example"
    (is (= 6 (part2 example3))))
  (testing "my input"
    (is (= 17099847107071 (part2 @my-input)))))
