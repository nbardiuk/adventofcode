(ns day23-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [day23 :refer [part1 part2]]))

(def example
  "#############
#...........#
###B#C#B#D###
  #A#D#C#A#
  #########")

(def my-input
  "#############
#...........#
###D#C#A#B###
  #B#C#D#A#
  #########")

(deftest part1-test
  (testing "example"
    (is (= 12521 (part1 example))))
  #_(testing "my input"
      (is (= 15160 (part1 my-input)))))

(deftest part2-test
  #_(testing "example"
      (is (= 44169 (part2 example))))
  #_(testing "my input"
      (is (= 46772 (part2 my-input)))))
