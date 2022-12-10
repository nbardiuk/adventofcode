(ns day10-test
  (:require
   [clojure.java.io :as io]
   [clojure.test :refer [deftest is testing]]
   [day10 :refer [part1 part2]]))

(def my-input
  (slurp (io/resource "input10.txt")))
(def example
  (slurp (io/resource "example10.txt")))

(deftest part1-test
  (testing "example"
    (is (= 13140 (part1 example))))
  (testing "my input"
    (is (= 11220 (part1 my-input)))))

(deftest part2-test
  (testing "example"
    (is (= ["##..##..##..##..##..##..##..##..##..##.."
            "###...###...###...###...###...###...###."
            "####....####....####....####....####...."
            "#####.....#####.....#####.....#####....."
            "######......######......######......####"
            "#######.......#######.......#######....."]
           (part2 example))))
  (testing "my input"
    (is (= ["###..####.###...##....##.####.#....#..#."
            "#..#....#.#..#.#..#....#.#....#....#.#.."
            "###....#..#..#.#..#....#.###..#....##..."
            "#..#..#...###..####....#.#....#....#.#.."
            "#..#.#....#....#..#.#..#.#....#....#.#.."
            "###..####.#....#..#..##..####.####.#..#."]
           (part2 my-input)))))
