(ns day20-test
  (:require
   [clojure.java.io :as io]
   [clojure.test :refer [deftest is testing]]
   [day20 :refer [part1 part2]]))

(def my-input (slurp (io/resource "input20.txt")))
(def example "1 2 -3 3 -2 0 4")

(deftest part1-test
  (testing "example"
    (is (= 3 (part1 example))))
  (testing "my input"
    (is (= 11616 (part1 my-input)))))

(deftest part2-test
  (testing "example"
    (is (= 1623178306 (part2 example))))
  (testing "my input"
    (is (= 9937909178485 (part2 my-input)))))
