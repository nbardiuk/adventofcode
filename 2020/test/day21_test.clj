(ns day21-test
  (:require [clojure.test :refer (is testing deftest)]
            [clojure.java.io :as io]
            [day21 :as sut]))

(def example
  "mxmxvkd kfcds sqjhc nhms (contains dairy, fish)
trh fvjkl sbzzf mxmxvkd (contains dairy)
sqjhc fvjkl (contains soy)
sqjhc mxmxvkd sbzzf (contains fish)")

(def my-input (slurp (io/resource "input21.txt")))

(deftest part1
  (testing "example"
    (is (= 5 (sut/part1 example))))
  (testing "my input"
    (is (= 2798 (sut/part1 my-input)))))

(deftest part2
  (testing "example"
    (is (= "mxmxvkd,sqjhc,fvjkl"
           (sut/part2 example))))
  (testing "my input"
    (is (= "gbt,rpj,vdxb,dtb,bqmhk,vqzbq,zqjm,nhjrzzj"
           (sut/part2 my-input)))))
