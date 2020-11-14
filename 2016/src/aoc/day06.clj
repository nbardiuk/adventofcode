(ns aoc.day06
  (:require [clojure.string :refer [split-lines]]))

(defn- transpose [xs]
  (let [result (map vector (first xs))]
    (reduce #(map conj %1 %2) result (rest xs))))

(defn- most-frequent [xs]
  (->> xs
       frequencies
       (apply max-key second)
       first))

(defn- least-frequent [xs]
  (->> xs
       frequencies
       (apply min-key second)
       first))

(defn part1 [s]
  (->> (split-lines s)
       transpose
       (map most-frequent)
       (apply str)))

(defn part2 [s]
  (->> (split-lines s)
       transpose
       (map least-frequent)
       (apply str)))
