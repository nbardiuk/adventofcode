(ns aoc.day03
  (:require [clojure.string :as cs]))

(defn triangle? [nums]
  (let [[a b c] (sort nums)]
    (> (+ a b) c)))

(defn parse-numbers [input]
  (let [lines (cs/split-lines input)
        lines (map cs/trim lines)
        words (map #(cs/split % #"\s+") lines)
        nums (map #(map read-string %) words)]
    nums))

(defn part1 [input]
  (let [nums (parse-numbers input)]
    (count (filter triangle? nums))))

(defn part2 [input]
  (let [nums (parse-numbers input)
        by-cols (flatten (map (fn [i] (map #(nth % i) nums)) (range 3)))
        tripples (partition 3 by-cols)]
    (count (filter triangle? tripples))))
