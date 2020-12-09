(ns day09-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [day09 :as sut]))

(def example
  "35
20
15
25
47
40
62
55
65
95
102
117
150
182
127
219
299
277
309
576")

(def my-input (slurp (io/resource "input9.txt")))

(deftest part1
  (testing "example"
    (is (= 127 (sut/part1 example 5))))
  (testing "my input"
    (is (= 25918798 (sut/part1 my-input 25)))))

(deftest part2
  (testing "example"
    (is (= 62 (sut/part2 example 5))))
  (testing "my input"
    (is (= 3340942 (sut/part2 my-input 25)))))
