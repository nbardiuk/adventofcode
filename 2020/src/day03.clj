(ns day03
  (:require [clojure.string :refer [split-lines]]))

(defn- cell [grid x y]
  (let [line (get grid y)]
    (get line (rem x (count line)))))

(defn- count-trees [grid [dx dy]]
  (let [xs (iterate #(+ dx %) 0)
        ys (range 0 (count grid) dy)]
    (->> (map #(cell grid %1 %2) xs ys)
         (filter #{\#})
         count)))

(defn- solution [slopes input]
  (let [grid (split-lines input)]
    (->> slopes
         (map #(count-trees grid %))
         (reduce *))))

(def part1 #(solution [[3 1]] %))
(def part2 #(solution [[1 1] [3 1] [5 1] [7 1] [1 2]] %))
