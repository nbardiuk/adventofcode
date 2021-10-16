(ns aoc.day07-test
  (:require [clojure.test :refer [deftest is]]
            [aoc.day07 :as sut]))

(def example-part1
  "abba[mnop]qrst
abcd[bddb]xyyx
aaaa[qwer]tyui
ioxxoj[asdfgh]zxcvbn")

(def example-part2
  "aba[bab]xyz
xyx[xyx]xyx
aaa[kek]eke
zazbz[bzb]cdb")

(def my-input (slurp "./resources/day07.txt"))

(deftest day7-part1-example
  (is (= 2 (sut/part1 example-part1))))

(deftest day7-part1-my-input
  (is (= 110 (sut/part1 my-input))))

(deftest day7-part2-example
  (is (= 3 (sut/part2 example-part2))))

(deftest day7-part2-my-input
  (is (= 242 (sut/part2 my-input))))
