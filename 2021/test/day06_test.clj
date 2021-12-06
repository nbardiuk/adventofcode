(ns day06-test
  (:require
   [clojure.java.io :as io]
   [clojure.test :refer [deftest is testing]]
   [day06 :refer [part1 part2]]))

(def example "3,4,3,1,2")
(def my-input (slurp (io/resource "input6.txt")))

(deftest part1-test
  (testing "example"
    (is (= 5934 (part1 example))))
  (testing "my input"
    (is (= 388739 (part1 my-input)))))

(deftest part2-test
  (testing "example"
    (is (= 26984457539 (part2 example))))
  (testing "my input"
    (is (= 1741362314973 (part2 my-input)))))
