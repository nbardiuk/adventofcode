(ns day08-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [day08 :as sut]))

(def example
  "nop +0
acc +1
jmp +4
acc +3
jmp -3
acc -99
acc +1
jmp -4
acc +6")

(def my-input (slurp (io/resource "input8.txt")))

(deftest part1
  (testing "example"
    (is (= 5 (sut/part1 example))))
  (testing "my input"
    (is (= 2034 (sut/part1 my-input)))))

(deftest part2
  (testing "example"
    (is (= 8 (sut/part2 example))))
  (testing "my input"
    (is (= 672 (sut/part2 my-input)))))
