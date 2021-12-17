(ns day17-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [day17 :refer [part1 part2]]))

(def example "target area: x=20..30, y=-10..-5")
(def my-input "target area: x=195..238, y=-93..-67")

(deftest part1-test
  (testing "example"
    (is (= 45 (part1 example))))
  (testing "my input"
    (is (= 4278 (part1 my-input)))))

(deftest part2-test
  (testing "example"
    (is (= 112 (part2 example))))
  (testing "my input"
    (is (= 1994 (part2 my-input)))))
