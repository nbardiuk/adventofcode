(ns day14-test
  (:require
   [clojure.java.io :as io]
   [clojure.test :refer [deftest is testing]]
   [day14 :refer [part1 part2]]))

(def my-input
  (slurp (io/resource "input14.txt")))
(def example
  "498,4 -> 498,6 -> 496,6
503,4 -> 502,4 -> 502,9 -> 494,9")

(deftest part1-test
  (testing "example"
    (is (= 24 (part1 example))))
  (testing "my input"
    (is (= 737 (part1 my-input)))))

(deftest part2-test
  (testing "example"
    (is (= 93 (part2 example))))
  (testing "my input"
    (is (= 28145 (part2 my-input)))))
