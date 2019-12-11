(ns aoc.day02
  (:require [clojure.string :as cs]))

(def simple-keyboard
  [[\1 \2 \3]
   [\4 \5 \6]
   [\7 \8 \9]])

(def complex-keyboard
  [[nil nil \1 nil nil]
   [nil \2  \3 \4  nil]
   [\5  \6  \7 \8  \9]
   [nil \A  \B \C  nil]
   [nil nil \D nil nil]])

(defn label [board [x y]]
  (-> board (get y) (get x)))

(defn move [board [x y :as position] direction]
  (let [candidate (case direction
                    \U [x (- y 1)]
                    \D [x (+ y 1)]
                    \L [(- x 1) y]
                    \R [(+ x 1) y])]
    (if (label board candidate) candidate position)))

(defn moves [board position directions]
  (reduce (partial move board) position directions))

(defn decode [input board start]
  (let [lines  (cs/split-lines input)
        points (reductions (partial moves board) start lines)
        labels (drop 1 (map (partial label board) points))]
    (apply str labels)))

(defn part1 [input]
  (decode input simple-keyboard [1 1]))

(defn part2 [input]
  (decode input complex-keyboard [0 2]))
