(ns day20-test
  (:require [clojure.test :refer (is testing deftest)]
            [clojure.java.io :as io]
            [day20 :as sut]))

(def my-input (slurp (io/resource "input20.txt")))
(def example (slurp (io/resource "example20.txt")))

(deftest part1
  (testing "example"
    (is (= 20899048083289 (sut/part1 example))))
  (testing "my input"
    (is (= 13224049461431 (sut/part1 my-input)))))

(deftest part2
  (testing "example"
    (is (= 273 (sut/part2 example))))
  (testing "my input"
    (is (= 2231 (sut/part2 my-input)))))
