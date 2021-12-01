(ns day01
  (:require
   [clojure.string :as string]))

(defn- parse-longs [input]
  (->> (string/split-lines input)
       (map parse-long)))

(defn- keep-increasing [numbers]
  (->> numbers
       (partition 2 1)
       (filter #(apply < %))
       (map second)))

(defn- three-measurements [numbers]
  (->> numbers
       (partition-all 3 1)
       (map #(apply + %))))

(defn part1 [input]
  (->> input
       parse-longs
       keep-increasing
       count))

(defn part2 [input]
  (->> input
       parse-longs
       three-measurements
       keep-increasing
       count))

