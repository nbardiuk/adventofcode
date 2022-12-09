(ns day09-test
  (:require
   [clojure.java.io :as io]
   [clojure.test :refer [deftest is testing]]
   [day09 :refer [part1 part2]]))

(def my-input
  (slurp (io/resource "input09.txt")))

(def example
  "R 4
U 4
L 3
D 1
R 4
D 1
L 5
R 2")
(def larger-example
  "R 5
U 8
L 8
D 3
R 17
D 10
L 25
U 20")

(deftest part1-test
  (testing "examples"
    (is (= 13 (part1 example)))
    (is (= 88 (part1 larger-example))))
  (testing "my input"
    (is (= 6087 (part1 my-input)))))

(deftest part2-test
  (testing "examples"
    (is (= 1 (part2 example)))
    (is (= 36 (part2 larger-example))))
  (testing "my input"
    (is (= 2493 (part2 my-input)))))
