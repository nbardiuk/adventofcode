(ns day25-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [day25 :as sut]))

(def example "5764801\n17807724")
(def my-input (slurp (io/resource "input25.txt")))

(deftest part1
  (testing "example"
    (is (= 14897079 (sut/part1 example))))
  (testing "my input"
    (is (= 11288669 (sut/part1 my-input)))))
