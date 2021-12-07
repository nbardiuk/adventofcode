(ns day07-test
  (:require
   [clojure.java.io :as io]
   [clojure.test :refer [deftest is testing]]
   [day07 :refer [part1 part2]]))

(def example "16,1,2,0,4,2,7,1,2,14")
(def my-input (slurp (io/resource "input7.txt")))

(deftest part1-test
  (testing "example"
    (is (= 37 (part1 example))))
  (testing "my input"
    (is (= 355989 (part1 my-input)))))

(deftest part2-test
  (testing "example"
    (is (= 168 (part2 example))))
  (testing "my input"
    (is (= 102245489 (part2 my-input)))))
