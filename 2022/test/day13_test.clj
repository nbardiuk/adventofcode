(ns day13-test
  (:require
   [clojure.java.io :as io]
   [clojure.test :refer [deftest is testing]]
   [day13 :refer [part1 part2]]))

(def my-input
  (slurp (io/resource "input13.txt")))

(def example
  (slurp (io/resource "example13.txt")))

(deftest part1-test
  (testing "example"
    (is (= 13 (part1 example))))
  (testing "my input"
    (is (= 5252 (part1 my-input)))))

(deftest part2-test
  (testing "example"
    (is (= 140 (part2 example))))
  (testing "my input"
    (is (= 20592 (part2 my-input)))))
