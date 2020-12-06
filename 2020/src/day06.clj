(ns day06
  (:require [clojure.string :refer [split split-lines]]
            [clojure.set :refer [union intersection]]))

(defn- answers [joining group]
  (->> group
       split-lines
       (map set)
       (reduce joining)
       count))

(defn- checksum [joining input]
  (->> (split input #"\R\R")
       (map #(answers joining %))
       (reduce +)))

(def part1 #(checksum union %))
(def part2 #(checksum intersection %))
