(ns day22-test
  (:require
   [clojure.java.io :as io]
   [clojure.test :refer [deftest is testing]]
   [day22 :refer [part1 part2]]))

(def my-input
  (slurp (io/resource "input22.txt")))
(def example
  (slurp (io/resource "example22.txt")))

(deftest part1-test
  (testing "example"
    (is (= 6032 (part1 example))))
  (testing "my input"
    (is (= 146092 (part1 my-input)))))

(deftest part2-test
  #_(testing "example"
      (is (= nil (part2 example))))
  (testing "my input"
    (is (= 110342 (part2 my-input)))))
