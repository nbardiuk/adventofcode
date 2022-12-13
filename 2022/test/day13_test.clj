(ns day13-test
  (:require
   [clojure.java.io :as io]
   [clojure.test :refer [deftest is testing]]
   [day13 :refer [part1 part2]]))

(def my-input
  (slurp (io/resource "input13.txt")))

(def example
  "[1,1,3,1,1]
[1,1,5,1,1]

[[1],[2,3,4]]
[[1],4]

[9]
[[8,7,6]]

[[4,4],4,4]
[[4,4],4,4,4]

[7,7,7,7]
[7,7,7]

[]
[3]

[[[]]]
[[]]

[1,[2,[3,[4,[5,6,7]]]],8,9]
[1,[2,[3,[4,[5,6,0]]]],8,9]")

(deftest part1-test
  (testing "example"
    (is (= 13 (part1 example))))
  (testing "my input"
    (is (= 5252 (part1 my-input)))))

(deftest part2-test
  (testing "example"
    (is (= 140 (part2 example))))
  (testing "my input"
    (is (= 20592 (part2 my-input)))))
