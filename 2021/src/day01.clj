(ns day01
  (:require
   [clojure.string :as string]))

(defn- parse-longs [input]
  (->> (string/split-lines input)
       (map parse-long)))

(defn- increasing-windows [window numbers]
  (->> (partition window 1 numbers)
       (filter #(< (first %) (last %)))))

(defn solution [window input]
  (->> (parse-longs input)
       (increasing-windows window)
       count))

(def part1 (partial solution 2))
(def part2 (partial solution 4))

