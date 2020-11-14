(ns aoc.day05-test
  (:require [clojure.test :refer [deftest is testing]]
            [aoc.day05 :as sut]))

(def input "ojvtpuvg")

(deftest day5-part1
  (testing "example"
    (is (= "18f47a30"
           (sut/part1 "abc"))))
  (testing "my input"
    (is (= "4543c154"
           (sut/part1 input)))))

(deftest day5-part2
  (testing "example"
    (is (= "05ace8e3"
           (sut/part2 "abc"))))
  (testing "my input"
    (is (= "1050cbbd"
           (sut/part2 input)))))
