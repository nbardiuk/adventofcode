(ns day15-test
  (:require [clojure.test :refer [deftest is testing]]
            [day15 :as sut]))

(deftest part1
  (testing "example"
    (is (= 436 (sut/part1 "0,3,6"))))
  (testing "my input"
    (is (= 1238 (sut/part1 "9,6,0,10,18,2,1")))))

(deftest part2
  (testing "example"
    (is (= 175594 (sut/part2 "0,3,6"))))
  (testing "my input"
    (is (= 3745954 (sut/part2 "9,6,0,10,18,2,1")))))
