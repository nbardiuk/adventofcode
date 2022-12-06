(ns day06-test
  (:require
   [clojure.java.io :as io]
   [clojure.test :refer [deftest is testing]]
   [day06 :refer [part1 part2]]))

(def my-input
  (slurp (io/resource "input06.txt")))

(deftest part1-test
  (testing "examples"
    (is (= 7 (part1 "mjqjpqmgbljsphdztnvjfqwrcgsmlb")))
    (is (= 5 (part1 "bvwbjplbgvbhsrlpgdmjqwftvncz")))
    (is (= 6 (part1 "nppdvjthqldpwncqszvftbrmjlhg")))
    (is (= 10 (part1 "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg")))
    (is (= 11 (part1 "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw"))))
  (testing "my input"
    (is (= 1300 (part1 my-input)))))

(deftest part2-test
  (testing "examples"
    (is (= 19 (part2 "mjqjpqmgbljsphdztnvjfqwrcgsmlb")))
    (is (= 23 (part2 "bvwbjplbgvbhsrlpgdmjqwftvncz")))
    (is (= 23 (part2 "nppdvjthqldpwncqszvftbrmjlhg")))
    (is (= 29 (part2 "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg")))
    (is (= 26 (part2 "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw"))))
  (testing "my input"
    (is (= 3986 (part2 my-input)))))
