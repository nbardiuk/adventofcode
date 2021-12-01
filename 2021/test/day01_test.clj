(ns day01-test
  (:require
   [clojure.java.io :as io]
   [clojure.test :refer [deftest is testing]]
   [day01 :refer [part1 part2]]))

(def example "199
200
208
210
200
207
240
269
260
263")

(def my-input (slurp (io/resource "input1.txt")))

(deftest part1-test
  (testing "example"
    (is (= 7 (part1 example))))
  (testing "my input"
    (is (= 1184 (part1 my-input)))))

(deftest part2-test
  (testing "example"
    (is (= 5 (part2 example))))
  (testing "my input"
    (is (= 1158 (part2 my-input)))))
