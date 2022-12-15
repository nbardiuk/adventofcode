(ns day15-test
  (:require
   [clojure.java.io :as io]
   [clojure.test :refer [deftest is testing]]
   [day15 :refer [part1 part2]]))

(def my-input
  (slurp (io/resource "input15.txt")))

(def example
  (slurp (io/resource "example15.txt")))

(deftest part1-test
  (testing "example"
    (is (= 26 (part1 10 example))))
  (testing "my input"
    (is (= 5256611 (part1 2000000 my-input)))))

(deftest part2-test
  (testing "example"
    (is (= 56000011 (part2 20 example))))
  (testing "my input"
    (is (= 13337919186981 (part2 4000000 my-input)))))
