(ns day23-test
  (:require [clojure.test :refer (is testing deftest)]
            [day23 :as sut]))

(def example [3 8 9 1 2 5 4 6 7])
(def my-input [3 2 7 4 6 5 1 8 9])

(deftest part1
  (testing "example"
    (is (= "67384529" (sut/part1 example))))
  (testing "my input"
    (is (= "82934675" (sut/part1 my-input)))))

(deftest part2
  (testing "example"
    (is (= 149245887792 (sut/part2 example))))
  (testing "my input"
    (is (= 474600314018 (sut/part2 my-input)))))
