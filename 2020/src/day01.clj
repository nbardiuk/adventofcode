(ns day01
  (:require [clojure.string :as s]))

(defn- parse-ints [input]
  (->> input
       s/split-lines
       (map #(Integer/parseInt %))))

(defn- sum [xs]
  (reduce + xs))

(defn- product [xs]
  (reduce * xs))

(defn- combinations [n xs]
  (loop [i 2
         rs (map vector xs)]
    (let [rs (for [x xs r rs] (conj r x))]
      (if (< i n)
        (recur (inc i) rs)
        rs))))

(defn find-first [p xs]
  (->> xs (filter p) first))

(defn- solution [n input]
  (->> input
       parse-ints
       (combinations n)
       (find-first #(= 2020 (sum %)))
       product))

(def part1 #(solution 2 %))
(def part2 #(solution 3 %))
