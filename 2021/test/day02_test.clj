(ns day02-test
  (:require
   [clojure.java.io :as io]
   [clojure.test :refer [deftest is testing]]
   [day02 :refer [part1 part2]]))

(def example
  "forward 5
down 5
forward 8
up 3
down 8
forward 2")

(def my-input (slurp (io/resource "input2.txt")))

(deftest part1-test
  (testing "example"
    (is (= 150 (part1 example))))
  (testing "my input"
    (is (= 2120749 (part1 my-input)))))

(deftest part2-test
  (testing "example"
    (is (= 900 (part2 example))))
  (testing "my input"
    (is (= 2138382217 (part2 my-input)))))
