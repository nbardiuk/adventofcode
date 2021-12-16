(ns day16-test
  (:require
   [clojure.java.io :as io]
   [clojure.test :refer [deftest is testing]]
   [day16 :refer [part1 part2]]))

(def my-input (slurp (io/resource "input16.txt")))

(deftest part1-test
  (testing "examples"
    (is (= 6 (part1 "D2FE28")))
    (is (= 16 (part1 "8A004A801A8002F478")))
    (is (= 12 (part1 "620080001611562C8802118E34")))
    (is (= 23 (part1 "C0015000016115A2E0802F182340")))
    (is (= 31 (part1 "A0016C880162017C3686B18A3D4780"))))
  (testing "my input"
    (is (= 949 (part1 my-input)))))

(deftest part2-test
  (testing "example"
    (is (= 3 (part2 "C200B40A82")))
    (is (= 54 (part2 "04005AC33890")))
    (is (= 7 (part2 "880086C3E88112")))
    (is (= 9 (part2 "CE00C43D881120")))
    (is (= 1 (part2 "D8005AC2A8F0")))
    (is (= 0 (part2 "F600BC2D8F")))
    (is (= 0 (part2 "9C005AC2F8F0")))
    (is (= 1 (part2 "9C0141080250320F1802104A08"))))
  (testing "my input"
    (is (= 1114600142730 (part2 my-input)))))
