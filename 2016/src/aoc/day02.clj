(ns aoc.day02
  (:require [clojure.string :as cs]))

(def keyboard
  [[1 2 3]
   [4 5 6]
   [7 8 9]])

(defn digit [[x y]]
  (get (get keyboard y) x))

(defn move [[x y] direction]
  (case direction
    \U [x (max (- y 1) 0)]
    \D [x (min (+ y 1) 2)]
    \L [(max (- x 1) 0) y]
    \R [(min (+ x 1) 2) y]))

(defn moves [position directions]
  (reduce move position directions))

(defn part1 [input]
  (let [lines  (cs/split-lines input)
        points (reductions moves [1 1] lines)
        digits (drop 1 (map digit points) )]
    (reduce (fn [r a] (+ (* r 10) a)) 0 digits)))
