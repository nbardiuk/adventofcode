(ns day01-test
  (:require
   [clojure.java.io :as io]
   [clojure.test :refer [deftest is testing]]
   [day01 :refer [part1 part2]]))

(def my-input
  (slurp (io/resource "input01.txt")))

(def example
  "1000
2000
3000

4000

5000
6000

7000
8000
9000

10000")

(deftest part1-test
  (testing "example"
    (is (= 24000 (part1 example))))
  (testing "my input"
    (is (= 70613 (part1 my-input)))))

(deftest part2-test
  (testing "example"
    (is (= 45000 (part2 example))))
  (testing "my input"
    (is (= 205805 (part2 my-input)))))
