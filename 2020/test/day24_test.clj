(ns day24-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [day24 :as sut]))

(def example
  "sesenwnenenewseeswwswswwnenewsewsw
neeenesenwnwwswnenewnwwsewnenwseswesw
seswneswswsenwwnwse
nwnwneseeswswnenewneswwnewseswneseene
swweswneswnenwsewnwneneseenw
eesenwseswswnenwswnwnwsewwnwsene
sewnenenenesenwsewnenwwwse
wenwwweseeeweswwwnwwe
wsweesenenewnwwnwsenewsenwwsesesenwne
neeswseenwwswnwswswnw
nenwswwsewswnenenewsenwsenwnesesenew
enewnwewneswsewnwswenweswnenwsenwsw
sweneswneswneneenwnewenewwneswswnese
swwesenesewenwneswnwwneseswwne
enesenwswwswneneswsenwnewswseenwsese
wnwnesenesenenwwnenwsewesewsesesew
nenewswnwewswnenesenwnesewesw
eneswnwswnwsenenwnwnwwseeswneewsenese
neswnwewnwnwseenwseesewsenwsweewe
wseweeenwnesenwwwswnew")

(def my-input (slurp (io/resource "input24.txt")))

(deftest part1
  (testing "example"
    (is (= 10 (sut/part1 example))))
  (testing "my input"
    (is (= 263 (sut/part1 my-input)))))

(deftest part2
  (testing "example"
    (is (= 2208 (sut/part2 example))))
  (testing "my input"
    (is (= 3649 (sut/part2 my-input)))))
