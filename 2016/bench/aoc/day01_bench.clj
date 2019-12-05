(ns aoc.day01-bench
  (:require [libra.bench :refer [defbench is]]
            [libra.criterium :refer [quick-bench]]
            [aoc.day01 :refer [part1 part2]]))

(def input (slurp "./resources/day01.txt"))

(defbench day01-part1 (is (quick-bench (part1 input))))
(defbench day01-part2 (is (quick-bench (part2 input))))
