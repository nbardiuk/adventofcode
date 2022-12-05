(ns day05-test
  (:require
   [clojure.java.io :as io]
   [clojure.test :refer [deftest is testing]]
   [day05 :refer [part1 part2]]))

(def my-input
  (slurp (io/resource "input05.txt")))

(def example
  "    [D]    
[N] [C]    
[Z] [M] [P]
 1   2   3 

move 1 from 2 to 1
move 3 from 1 to 3
move 2 from 2 to 1
move 1 from 1 to 2")

(deftest part1-test
  (testing "example"
    (is (= "CMZ" (part1 example))))
  (testing "my input"
    (is (= "JCMHLVGMG" (part1 my-input)))))

(deftest part2-test
  (testing "example"
    (is (= "MCD" (part2 example))))
  (testing "my input"
    (is (= "LVMRWSSPZ" (part2 my-input)))))
