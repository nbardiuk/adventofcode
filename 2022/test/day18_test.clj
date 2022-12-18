(ns day18-test
  (:require
   [clojure.java.io :as io]
   [clojure.test :refer [deftest is testing]]
   [day18 :refer [part1 part2]]))

(def my-input
  (slurp (io/resource "input18.txt")))

(def example
  "2,2,2
  1,2,2
  3,2,2
  2,1,2
  2,3,2
  2,2,1
  2,2,3
  2,2,4
  2,2,6
  1,2,5
  3,2,5
  2,1,5
  2,3,5")

(deftest part1-test
  (testing "example"
    (is (= 64 (part1 example))))
  (testing "my input"
    (is (= 4370 (part1 my-input)))))

(deftest part2-test
  (testing "example"
    (is (= 58 (part2 example))))
  (testing "my input"
    (is (= 2458 (part2 my-input)))))
