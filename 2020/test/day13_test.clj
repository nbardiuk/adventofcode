(ns day13-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [day13 :as sut]))

(def my-input (slurp (io/resource "input13.txt")))

(deftest part1
  (testing "example"
    (is (= 295 (sut/part1 "939\n7,13,x,x,59,x,31,19"))))
  (testing "my input"
    (is (= 115 (sut/part1 my-input)))))

(deftest part2
  (testing "example1"
    (is (= 1068781 (sut/part2 "n\n7,13,x,x,59,x,31,19"))))
  (testing "example2"
    (is (= 3417 (sut/part2 "n\n17,x,13,19"))))
  (testing "example3"
    (is (= 754018 (sut/part2 "n\n67,7,59,61"))))
  (testing "example4"
    (is (= 1202161486 (sut/part2 "n\n1789,37,47,1889"))))
  (testing "my input"
    (is (= 756261495958122 (sut/part2 my-input)))))
