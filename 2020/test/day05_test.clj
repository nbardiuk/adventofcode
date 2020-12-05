(ns day05-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [day05 :as sut]))

(def my-input (slurp (io/resource "input5.txt")))

(deftest part1
  (testing "examples"
    (is (= 820 (sut/part1 "BFFFBBFRRR\nFFFBBBFRRR\nBBFFBBFRLL"))))
  (testing "my input"
    (is (= 991 (sut/part1 my-input)))))

(deftest part2
  (testing "my input"
    (is (= 534 (sut/part2 my-input)))))
