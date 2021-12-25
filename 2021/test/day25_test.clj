(ns day25-test
  (:require
   [clojure.java.io :as io]
   [clojure.test :refer [deftest is testing]]
   [day25 :refer [part1]]))

(def example
  "v...>>.vv>
.vv>>.vv..
>>.>v>...v
>>v>>.>.v.
v>v.vv.v..
>.>>..v...
.vv..>.>v.
v.v..>>v.v
....v..v.>")

(def my-input (slurp (io/resource "input25.txt")))

(deftest part1-test
  (testing "example"
    (is (= 58 (part1 example))))
  (testing "my input"
    (is (= 471 (part1 my-input)))))
