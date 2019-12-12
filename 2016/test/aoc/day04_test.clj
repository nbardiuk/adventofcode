(ns aoc.day04-test
  (:require [aoc.day04 :refer [part1 part2]]
            [clojure.test :refer [deftest is testing]]))

(def input (slurp "./resources/day04.txt"))

(deftest day4-part1
  (testing "example"
    (let [example "aaaaa-bbb-z-y-x-123[abxyz]
a-b-c-d-e-f-g-h-987[abcde]
not-a-real-room-404[oarel]
totally-real-room-200[decoy]"]
      (is (= 1514 (part1 example)))))
  (testing "my input"
    (is (= 278221 (part1 input)))))

(deftest day4-part2
  (testing "my input"
    (is (= 267 (part2 input)))))
