(ns day15-test
  (:require
   [clojure.java.io :as io]
   [clojure.test :refer [deftest is testing]]
   [day15 :refer [part1 part2]]))

(def example
  "1163751742
1381373672
2136511328
3694931569
7463417111
1319128137
1359912421
3125421639
1293138521
2311944581")

(def my-input (slurp (io/resource "input15.txt")))

(deftest part1-test
  (testing "example"
    (is (= 40 (part1 example))))
  (testing "my input"
    (is (= 687 (part1 my-input)))))

(deftest part2-test
  (testing "example"
    (is (= 315 (part2 example))))
  (testing "my input"
    (is (= 2957 (part2 my-input)))))
