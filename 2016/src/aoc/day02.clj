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

(defn label [keyboard [x y]]
  (get (get keyboard y) x))

(defn move [keyboard [x y] direction]
  (let [next-position (case direction
                        \U [x (- y 1)]
                        \D [x (+ y 1)]
                        \L [(- x 1) y]
                        \R [(+ x 1) y])]
    (if (label keyboard next-position) next-position [x y])))

(defn moves [keyboard position directions]
  (reduce (partial move keyboard) position directions))

(defn decode [input keyboard start]
  (let [lines  (cs/split-lines input)
        points (reductions (partial moves keyboard) start lines)
        labels (drop 1 (map (partial label keyboard) points) )]
    (apply str labels)))

(defn part1 [input]
  (decode input simple-keyboard [1 1]))

(defn part2 [input]
  (decode input complex-keyboard [0 2]))
