(ns day21-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [day21 :refer [part1 part2]]))

(def example
  "Player 1 starting position: 4
Player 2 starting position: 8")

(def my-input
  "Player 1 starting position: 7
Player 2 starting position: 3")

(deftest part1-test
  (testing "example"
    (is (= 739785 (part1 example))))
  (testing "my input"
    (is (= 551901 (part1 my-input)))))

(deftest part2-test
  (testing "example"
    (is (= 444356092776315 (part2 example))))
  (testing "my input"
    (is (= 272847859601291 (part2 my-input)))))
