(ns aoc.day01
  (:require [clojure.string :as cs]
            ))

(defn words [line] (cs/split line #", "))

(defn parse-command [word]
  (let [[direction distance] (split-at 1 word)]
    [(apply str direction ) (read-string (apply str distance ))]))

(defn parse-commands [line] 
  (map parse-command (words line)))

(defn navigate [[[x y] orientation] [direction distance]] 
  (case [orientation direction] 
    ["N" "R"] [[( + x distance ) y] , "E"]
    ["N" "L"] [[( - x distance ) y] , "W"]
    ["E" "R"] [[x (+ y distance)] , "S"]
    ["E" "L"] [[x (- y distance)] , "N"]
    ["S" "R"] [[( - x distance ) y] , "W"]
    ["S" "L"] [[( + x distance ) y] , "E"]
    ["W" "R"] [[x (- y distance)] , "N"]
    ["W" "L"] [[x (+ y distance)] , "S"]
    [[x y] , orientation]
    ))

(defn part1 [input] 
  (let [commands (parse-commands input)
        [[x y] _ ] (reduce navigate [[0 0] "N"] commands) ]
    (+ (Math/abs x) (Math/abs y) )))
