(ns day21-test
  (:require
   [clojure.java.io :as io]
   [clojure.test :refer [deftest is testing]]
   [day21 :refer [part1 part2]]))

(def my-input
  (slurp (io/resource "input21.txt")))
(def example
  "root: pppw + sjmn
dbpl: 5
cczh: sllz + lgvd
zczc: 2
ptdq: humn - dvpt
dvpt: 3
lfqf: 4
humn: 5
ljgn: 2
sjmn: drzm * dbpl
sllz: 4
pppw: cczh / lfqf
lgvd: ljgn * ptdq
drzm: hmdt - zczc
hmdt: 32")

(deftest part1-test
  (testing "example"
    (is (= 152 (part1 example))))
  (testing "my input"
    (is (= 21208142603224 (part1 my-input)))))

(deftest part2-test
  (testing "example"
    (is (= 301 (part2 example))))
  (testing "my input"
    (is (= 3882224466191 (part2 my-input)))))
