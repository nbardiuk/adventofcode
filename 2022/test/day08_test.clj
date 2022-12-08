(ns day08-test
  (:require
   [clojure.java.io :as io]
   [clojure.test :refer [deftest is testing]]
   [day08 :refer [part1 part2]]))

(def my-input
  (slurp (io/resource "input08.txt")))

(def example
  "30373
25512
65332
33549
35390")

(deftest part1-test
  (testing "example"
    (is (= 21 (part1 example))))
  (testing "my input"
    (is (= 1801 (part1 my-input)))))

(deftest part2-test
  (testing "example"
    (is (= 8 (part2 example))))
  (testing "my input"
    (is (= 209880 (part2 my-input)))))
