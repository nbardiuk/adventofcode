(ns day18-test
  (:require
   [clojure.java.io :as io]
   [clojure.test :refer [deftest is testing]]
   [day18 :refer [part1 part2]]))

(def example
  "[[[0,[5,8]],[[1,7],[9,6]]],[[4,[1,2]],[[1,4],2]]]
[[[5,[2,8]],4],[5,[[9,9],0]]]
[6,[[[6,2],[5,6]],[[7,6],[4,7]]]]
[[[6,[0,7]],[0,9]],[4,[9,[9,0]]]]
[[[7,[6,4]],[3,[1,3]]],[[[5,5],1],9]]
[[6,[[7,3],[3,2]]],[[[3,8],[5,7]],4]]
[[[[5,4],[7,7]],8],[[8,3],8]]
[[9,3],[[9,9],[6,[4,9]]]]
[[2,[[7,7],7]],[[5,8],[[9,3],[0,2]]]]
[[[[5,2],5],[8,[3,7]]],[[5,[7,5]],[4,4]]]")

(def my-input (slurp (io/resource "input18.txt")))

(deftest part1-test
  (testing "example"
    (is (= 4140 (part1 example))))
  (testing "my input"
    (is (= 4176 (part1 my-input)))))

(deftest part2-test
  (testing "example"
    (is (= 3993 (part2 example))))
  (testing "my input"
    (is (= 4633 (part2 my-input)))))
