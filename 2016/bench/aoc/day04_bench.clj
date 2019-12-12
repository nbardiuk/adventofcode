(ns aoc.day04-bench
  (:require [aoc.day04 :refer [part1 part2]]
            [libra.bench :refer [defbench is]]
            [libra.criterium :refer [quick-bench]]))

(def input (slurp "./resources/day04.txt"))

(defbench day04-part1 (is (quick-bench (part1 input))))
(defbench day04-part2 (is (quick-bench (part2 input))))
