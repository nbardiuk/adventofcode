(ns day20-test
  (:require
   [clojure.java.io :as io]
   [clojure.test :refer [deftest is testing]]
   [day20 :refer [part1 part2]]))

(def example (slurp (io/resource "example20.txt")))
(def my-input (slurp (io/resource "input20.txt")))

(deftest part1-test
  (testing "example"
    (is (= 35 (part1 example))))
  (testing "my input"
    (is (= 5786 (part1 my-input)))))

(deftest part2-test
  (testing "example"
    (is (= 3351 (part2 example))))
  (testing "my input"
    (is (= 16757 (part2 my-input)))))
