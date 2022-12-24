(ns day24-test
  (:require
   [clojure.java.io :as io]
   [clojure.test :refer [deftest is testing]]
   [day24 :refer [part1 part2]]))

(def my-input
  (slurp (io/resource "input24.txt")))
(def example
  (slurp (io/resource "example24.txt")))

(deftest part1-test
  (testing "example"
    (is (= 18 (part1 example))))
  (testing "my input"
    (is (= 260 (part1 my-input)))))

(deftest part2-test
  (testing "example"
    (is (= 54 (part2 example))))
  (testing "my input"
    (is (= 747 (part2 my-input)))))
