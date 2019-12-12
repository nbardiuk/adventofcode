(ns aoc.day02-bench
  (:require [aoc.day02 :refer [part1 part2]]
            [libra.bench :refer [defbench is]]
            [libra.criterium :refer [quick-bench]]))

(def input (slurp "./resources/day02.txt"))

(defbench day02-part1 (is (quick-bench (part1 input))))
(defbench day02-part2 (is (quick-bench (part2 input))))
