(ns day19-test
  (:require
   [clojure.java.io :as io]
   [clojure.test :refer [deftest is testing]]
   [day19 :refer [part1 part2]]))

(def my-input
  (slurp (io/resource "input19.txt")))

(def example
  "Blueprint 1: Each ore robot costs 4 ore. Each clay robot costs 2 ore. Each obsidian robot costs 3 ore and 14 clay. Each geode robot costs 2 ore and 7 obsidian.
Blueprint 2: Each ore robot costs 2 ore. Each clay robot costs 3 ore. Each obsidian robot costs 3 ore and 8 clay. Each geode robot costs 3 ore and 12 obsidian.")

(deftest part1-test
  (testing "example"
    (is (= 33 (part1 example))))
  (testing "my input"
    (is (= 1306 (part1 my-input)))))

(deftest part2-test
  (testing "example"
    (is (= 3472 (part2 example))))
  (testing "my input"
    (is (= 37604 (part2 my-input)))))
