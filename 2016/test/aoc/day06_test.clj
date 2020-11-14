(ns aoc.day06-test
  (:require [clojure.test :refer [deftest is]]
            [aoc.day06 :as sut]))

(def example
  "eedadn
drvtee
eandsr
raavrd
atevrs
tsrnev
sdttsa
rasrtv
nssdts
ntnada
svetve
tesnvt
vntsnd
vrdear
dvrsen
enarar")

(def my-input (slurp "./resources/day06.txt"))

(deftest day6-part1-example
  (is (= "easter" (sut/part1 example))))

(deftest day6-part1-my-input
  (is (= "kqsdmzft" (sut/part1 my-input))))

(deftest day6-part2-example
  (is (= "advent" (sut/part2 example))))

(deftest day6-part2-my-input
  (is (= "tpooccyo" (sut/part2 my-input))))
