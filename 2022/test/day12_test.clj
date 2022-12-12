(ns day12-test
  (:require
   [clojure.java.io :as io]
   [clojure.test :refer [deftest is testing]]
   [day12 :refer [part1 part2]]))

(def my-input
  (slurp (io/resource "input12.txt")))

(def example
  "Sabqponm
abcryxxl
accszExk
acctuvwj
abdefghi")

(deftest part1-test
  (testing "example"
    (is (= 31 (part1 example))))
  (testing "my input"
    (is (= 383 (part1 my-input)))))

(deftest part2-test
  (testing "example"
    (is (= 29 (part2 example))))
  (testing "my input"
    (is (= 377 (part2 my-input)))))
