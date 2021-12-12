(ns day12-test
  (:require
   [clojure.java.io :as io]
   [clojure.test :refer [deftest is testing]]
   [day12 :refer [part1 part2]]))

(def example "start-A
start-b
A-c
A-b
b-d
A-end
b-end")

(def my-input (slurp (io/resource "input12.txt")))

(deftest part1-test
  (testing "example"
    (is (= 10 (part1 example))))
  (testing "my input"
    (is (= 5212 (part1 my-input)))))

(deftest part2-test
  (testing "example"
    (is (= 36 (part2 example))))
  (testing "my input"
    (is (= 134862 (part2 my-input)))))
