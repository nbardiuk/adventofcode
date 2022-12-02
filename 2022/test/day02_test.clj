(ns day02-test
  (:require
   [clojure.java.io :as io]
   [clojure.test :refer [deftest is testing]]
   [day02 :refer [part1 part2]]))

(def my-input
  (slurp (io/resource "input02.txt")))

(def example
  "A Y
B X
C Z")

(deftest part1-test
  (testing "example"
    (is (= 15 (part1 example))))
  (testing "my input"
    (is (= 15691 (part1 my-input)))))

(deftest part2-test
  (testing "example"
    (is (= 12 (part2 example))))
  (testing "my input"
    (is (= 12989 (part2 my-input)))))
