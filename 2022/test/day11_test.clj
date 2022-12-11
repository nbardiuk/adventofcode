(ns day11-test
  (:require
   [clojure.java.io :as io]
   [clojure.test :refer [deftest is testing]]
   [day11 :refer [part1 part2]]))

(def my-input
  (slurp (io/resource "input11.txt")))

(def example
  (slurp (io/resource "example11.txt")))

(deftest part1-test
  (testing "example"
    (is (= 10605 (part1 example))))
  (testing "my input"
    (is (= 66124 (part1 my-input)))))

(deftest part2-test
  (testing "example"
    (is (= 2713310158 (part2 example))))
  (testing "my input"
    (is (= 19309892877 (part2 my-input)))))
