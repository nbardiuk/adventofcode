(ns day25-test
  (:require
   [clojure.java.io :as io]
   [clojure.test :refer [deftest is testing]]
   [day25 :refer [part1]]))

(def my-input
  (slurp (io/resource "input25.txt")))

(def example
  "1=-0-2
12111
2=0=
21
2=01
111
20012
112
1=-1=
1-12
12
1=
122")

(deftest part1-test
  (testing "example"
    (is (= "2=-1=0" (part1 example))))
  (testing "my input"
    (is (= "20=02=120-=-2110-0=1" (part1 my-input)))))
