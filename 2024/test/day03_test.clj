(ns day03-test
  (:require
    [clojure.java.io :as io]
    [clojure.test :refer [deftest is testing]]
    [day03 :refer [part1 part2]]))

(def my-input
  (delay (slurp (io/resource "input03.txt"))))

(def example1
  "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))")

(def example2
  "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))")

(deftest part1-test
  (testing "example"
    (is (= 161 (part1 example1))))
  (testing "my input"
    (is (= 174960292 (part1 @my-input)))))

(deftest part2-test
  (testing "example"
    (is (= 48 (part2 example2))))
  (testing "my input"
    (is (= 56275602 (part2 @my-input)))))
