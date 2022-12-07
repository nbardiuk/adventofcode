(ns day07-test
  (:require
   [clojure.java.io :as io]
   [clojure.test :refer [deftest is testing]]
   [day07 :refer [part1 part2]]))

(def my-input
  (slurp (io/resource "input07.txt")))

(def example
  "$ cd /
$ ls
dir a
14848514 b.txt
8504156 c.dat
dir d
$ cd a
$ ls
dir e
29116 f
2557 g
62596 h.lst
$ cd e
$ ls
584 i
$ cd ..
$ cd ..
$ cd d
$ ls
4060174 j
8033020 d.log
5626152 d.ext
7214296 k")

(deftest part1-test
  (testing "example"
    (is (= 95437 (part1 example))))
  (testing "my input"
    (is (= 1749646 (part1 my-input)))))

(deftest part2-test
  (testing "example"
    (is (= 24933642 (part2 example))))
  (testing "my input"
    (is (= 1498966 (part2 my-input)))))
