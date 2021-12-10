(ns day10-test
  (:require
   [clojure.java.io :as io]
   [clojure.test :refer [deftest is testing]]
   [day10 :refer [part1 part2]]))

(def example "[({(<(())[]>[[{[]{<()<>>
[(()[<>])]({[<{<<[]>>(
{([(<{}[<>[]}>{[]{[(<()>
(((({<>}<{<{<>}{[]{[]{}
[[<[([]))<([[{}[[()]]]
[{[{({}]{}}([{[{{{}}([]
{<[[]]>}<{[{[{[]{()[[[]
[<(<(<(<{}))><([]([]()
<{([([[(<>()){}]>(<<{{
<{([{{}}[<[[[<>{}]]]>[]")

(def my-input (slurp (io/resource "input10.txt")))

(deftest part1-test
  (testing "example"
    (is (= 26397 (part1 example))))
  (testing "my input"
    (is (= 415953 (part1 my-input)))))

(deftest part2-test
  (testing "example"
    (is (= 288957 (part2 example))))
  (testing "my input"
    (is (= 2292863731 (part2 my-input)))))
