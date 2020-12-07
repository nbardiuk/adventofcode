(ns day07-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [day07 :as sut]))

(def example1
  "light red bags contain 1 bright white bag, 2 muted yellow bags.
dark orange bags contain 3 bright white bags, 4 muted yellow bags.
bright white bags contain 1 shiny gold bag.
muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.
shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.
dark olive bags contain 3 faded blue bags, 4 dotted black bags.
vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
faded blue bags contain no other bags.
dotted black bags contain no other bags.")

(def example2
  "shiny gold bags contain 2 dark red bags.
dark red bags contain 2 dark orange bags.
dark orange bags contain 2 dark yellow bags.
dark yellow bags contain 2 dark green bags.
dark green bags contain 2 dark blue bags.
dark blue bags contain 2 dark violet bags.
dark violet bags contain no other bags.")

(def my-input (slurp (io/resource "input7.txt")))

(deftest part1
  (testing "example"
    (is (= 4 (sut/part1 example1))))
  (testing "my input"
    (is (= 378 (sut/part1 my-input)))))

(deftest part2
  (testing "example 1"
    (is (= 32 (sut/part2 example1))))
  (testing "example 2"
    (is (= 126 (sut/part2 example2))))
  (testing "my input"
    (is (= 27526 (sut/part2 my-input)))))
