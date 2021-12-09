(ns day09-test
  (:require
   [clojure.java.io :as io]
   [clojure.test :refer [deftest is testing]]
   [day09 :refer [part1 part2]]))

(def example
  "2199943210
3987894921
9856789892
8767896789
9899965678")

(def my-input (slurp (io/resource "input9.txt")))

(deftest part1-test
  (testing "example"
    (is (= 15 (part1 example))))
  (testing "my input"
    (is (= 603 (part1 my-input)))))

(deftest part2-test
  (testing "example"
    (is (= 1134 (part2 example))))
  (testing "my input"
    (is (= 786780 (part2 my-input)))))
