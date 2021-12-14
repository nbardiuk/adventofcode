(ns day14-test
  (:require
   [clojure.java.io :as io]
   [clojure.test :refer [deftest is testing]]
   [day14 :refer [part1 part2]]))

(def example "NNCB

CH -> B
HH -> N
CB -> H
NH -> C
HB -> C
HC -> B
HN -> C
NN -> C
BH -> H
NC -> B
NB -> B
BN -> B
BB -> N
BC -> B
CC -> N
CN -> C")

(def my-input (slurp (io/resource "input14.txt")))

(deftest part1-test
  (testing "example"
    (is (= 1588 (part1 example))))
  (testing "my input"
    (is (= 2703 (part1 my-input)))))

(deftest part2-test
  (testing "example"
    (is (= 2188189693529 (part2 example))))
  (testing "my input"
    (is (= 2984946368465 (part2 my-input)))))
