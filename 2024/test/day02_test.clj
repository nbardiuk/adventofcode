(ns day02-test
  (:require
    [clojure.java.io :as io]
    [clojure.test :refer [deftest is testing]]
    [day02 :refer [part1 part2]]))


(def my-input
  (delay (slurp (io/resource "input02.txt"))))


(def example1
  "7 6 4 2 1
1 2 7 8 9
9 7 6 2 1
1 3 2 4 5
8 6 4 4 1
1 3 6 7 9")

(deftest part1-test
  (testing "example"
    (is (= 2 (part1 example1))))
  (testing "my input"
    (is (= 356 (part1 @my-input)))))

(deftest part2-test
    (testing "example"
      (is (= 4 (part2 example1))))
    (testing "my input"
      (is (= 413 (part2 @my-input)))))


