(ns day02-test
  (:require
    [clojure.java.io :as io]
    [clojure.test :refer [deftest is testing]]
    [day02 :refer [part1 part2]]))


(def my-input
  (delay (slurp (io/resource "input02.txt"))))


(def example
  "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green")


(deftest part1-test
  (testing "example"
    (is (= 8 (part1 example))))
  (testing "my input"
    (is (= 2776 (part1 @my-input)))))


(deftest part2-test
  (testing "example"
    (is (= 2286 (part2 example))))
  (testing "my input"
    (is (= 68638 (part2 @my-input)))))
