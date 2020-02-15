(ns aoc.day03
  (:require [clojure.string :as s]))

(defn- parse-numbers [input]
  (->> input
       s/split-lines
       (map s/trim)
       (map #(s/split % #"\s+"))
       (map #(map read-string %))))

(defn- triangle? [nums]
  (let [[a b c] (sort nums)]
    (> (+ a b) c)))

(defn- transpose [[row & rows]]
  (let [columns (map vector row)]
    (reduce (partial map conj) columns rows)))

(defn- tripples-by-columns [rows]
  (->> rows
       transpose
       (apply concat)
       (partition 3)))

(defn part1 [input]
  (->> input
       parse-numbers
       (filter triangle?)
       count))

(defn part2 [input]
  (->> input
       parse-numbers
       tripples-by-columns
       (filter triangle?)
       count))
