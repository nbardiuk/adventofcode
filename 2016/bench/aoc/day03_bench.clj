(ns aoc.day03-bench
  (:require [aoc.day03 :refer [part1 part2]]
            [libra.bench :refer [defbench is]]
            [libra.criterium :refer [quick-bench]]))

(def input (slurp "./resources/day03.txt"))

(defbench day03-part1 (is (quick-bench (part1 input))))
(defbench day03-part2 (is (quick-bench (part2 input))))
