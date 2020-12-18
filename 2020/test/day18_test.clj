(ns day18-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [day18 :as sut]))

(def my-input (slurp (io/resource "input18.txt")))

(deftest part1
  (testing "examples"
    (is (= 71 (sut/part1 "1 + 2 * 3 + 4 * 5 + 6")))
    (is (= 51 (sut/part1 "1 + (2 * 3) + (4 * (5 + 6))")))
    (is (= 26 (sut/part1 "2 * 3 + (4 * 5)")))
    (is (= 437 (sut/part1 "5 + (8 * 3 + 9 + 3 * 4 * 3)")))
    (is (= 12240 (sut/part1 "5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))")))
    (is (= 13632 (sut/part1 "((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2"))))
  (testing "my input"
    (is (= 3885386961962 (time (sut/part1 my-input))))))

(deftest part2
  (testing "examples"
    (is (= 231 (sut/part2 "1 + 2 * 3 + 4 * 5 + 6")))
    (is (= 51 (sut/part2 "1 + (2 * 3) + (4 * (5 + 6))")))
    (is (= 46 (sut/part2 "2 * 3 + (4 * 5)")))
    (is (= 1445 (sut/part2 "5 + (8 * 3 + 9 + 3 * 4 * 3)")))
    (is (= 669060 (sut/part2 "5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))")))
    (is (= 23340 (sut/part2 "((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2"))))
  (testing "my input"
    (is (= 112899558798666 (time (sut/part2 my-input))))))
