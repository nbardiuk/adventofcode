(ns day13-test
  (:require
   [clojure.java.io :as io]
   [clojure.test :refer [deftest is testing]]
   [day13 :refer [part1 part2]]))

(def example "6,10
0,14
9,10
0,3
10,4
4,11
6,0
6,12
4,1
0,13
10,12
3,4
3,0
8,4
1,10
2,14
8,10
9,0

fold along y=7
fold along x=5")

(def example-out
  "#####
#   #
#   #
#   #
#####")

(def my-input (slurp (io/resource "input13.txt")))

(def my-out
  "####  ##  #  # #  # ###  ####  ##  ### 
#    #  # #  # # #  #  # #    #  # #  #
###  #  # #### ##   #  # ###  #    #  #
#    #### #  # # #  ###  #    #    ### 
#    #  # #  # # #  # #  #    #  # #   
#### #  # #  # #  # #  # ####  ##  #   ")

(deftest part1-test
  (testing "example"
    (is (= 17 (part1 example))))
  (testing "my input"
    (is (= 827 (part1 my-input)))))

(deftest part2-test
  (testing "example"
    (is (= example-out (part2 example))))
  (testing "my input"
    (is (= my-out (part2 my-input)))))
