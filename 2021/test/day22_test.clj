(ns day22-test
  (:require
   [clojure.java.io :as io]
   [clojure.test :refer [deftest is testing]]
   [day22 :refer [part1 part2]]))

(def example (slurp (io/resource "example22.txt")))
(def my-input (slurp (io/resource "input22.txt")))

(deftest part1-test
  (testing "example"
    (is (= 474140 (part1 example))))
  (testing "my input"
    (is (= 553201 (part1 my-input)))))

(deftest part2-test
  (testing "example"
    (is (= 2758514936282235 (part2 example))))
  #_(testing "my input"
      (is (= 1263946820845866 (part2 my-input)))))
