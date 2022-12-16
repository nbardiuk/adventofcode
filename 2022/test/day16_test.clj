(ns day16-test
  (:require
   [clojure.java.io :as io]
   [clojure.test :refer [deftest is testing]]
   [day16 :refer [part1 part2]]))

(def my-input
  (slurp (io/resource "input16.txt")))

(def example
  "Valve AA has flow rate=0; tunnels lead to valves DD, II, BB
  Valve BB has flow rate=13; tunnels lead to valves CC, AA
  Valve CC has flow rate=2; tunnels lead to valves DD, BB
  Valve DD has flow rate=20; tunnels lead to valves CC, AA, EE
  Valve EE has flow rate=3; tunnels lead to valves FF, DD
  Valve FF has flow rate=0; tunnels lead to valves EE, GG
  Valve GG has flow rate=0; tunnels lead to valves FF, HH
  Valve HH has flow rate=22; tunnel leads to valve GG
  Valve II has flow rate=0; tunnels lead to valves AA, JJ
  Valve JJ has flow rate=21; tunnel leads to valve II")

(deftest part1-test
  (testing "example"
    (is (= 1651 (part1 example))))
  (testing "my input"
    (is (= 2080 (part1 my-input)))))

(deftest part2-test
  (testing "example"
    (is (= 1707 (part2 example))))
  (testing "my input"
    (is (= 2752 (part2 my-input)))))
