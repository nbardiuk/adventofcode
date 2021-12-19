(ns day19-test
  (:require
   [clojure.java.io :as io]
   [clojure.test :refer [deftest is testing]]
   [day19 :refer [part1 part2]]))

(def example (slurp (io/resource "example19.txt")))
(def my-input (slurp (io/resource "input19.txt")))

(deftest part1-test
  (testing "example"
    (is (= 79 (part1 example))))
  #_(testing "my input"
      (is (= 308 (time (part1 my-input))))))

(deftest part2-test
  (testing "example"
    (is (= 3621.0 (part2 example))))
  #_(testing "my input"
      (is (= 12124 (part2 my-input)))))
