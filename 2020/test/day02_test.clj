(ns day02-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [day02 :as sut]))

(def example
  "1-3 a: abcde
1-3 b: cdefg
2-9 c: ccccccccc")

(def my-input (slurp (io/resource "input02.txt")))

(deftest part1
  (testing "example"
    (is (= 2 (sut/part1 example))))
  (testing "my input"
    (is (= 536 (sut/part1 my-input)))))

(deftest part2
  (testing "example"
    (is (= 1 (sut/part2 example))))
  (testing "my input"
    (is (= 558 (sut/part2 my-input)))))
