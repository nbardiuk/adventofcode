(ns day04-test
  (:require
   [clojure.java.io :as io]
   [clojure.test :refer [deftest is testing]]
   [day04 :refer [part1 part2]]))

(def my-input
  (slurp (io/resource "input04.txt")))

(def example
  "2-4,6-8
2-3,4-5
5-7,7-9
2-8,3-7
6-6,4-6
2-6,4-8")

(deftest part1-test
  (testing "example"
    (is (= 2 (part1 example))))
  (testing "my input"
    (is (= 580 (part1 my-input)))))

(deftest part2-test
  (testing "example"
    (is (= 4 (part2 example))))
  (testing "my input"
    (is (= 895 (part2 my-input)))))
