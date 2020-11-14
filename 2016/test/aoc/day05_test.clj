(ns aoc.day05-test
  (:require [clojure.test :refer [deftest is testing]]
            [aoc.day05 :as sut]))

(def input "ojvtpuvg")

(deftest ^:slow day5-part1-example
  (is (= "18f47a30"
         (sut/part1 "abc"))))

(deftest ^:slow day5-part1-my-input
  (is (= "4543c154"
         (sut/part1 input))))

(deftest ^:slow day5-part2-example
  (is (= "05ace8e3"
         (sut/part2 "abc"))))

(deftest ^:slow day5-part2-my-input
  (is (= "1050cbbd"
         (sut/part2 input))))
