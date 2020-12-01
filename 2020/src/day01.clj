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
         result (map vector xs)]
    (let [result (for [a xs r result] (conj r a))]
      (if (< i n)
        (recur (inc i) result)
        result))))

(defn- solution [n input]
  (->> input
       parse-ints
       (combinations n)
       (some #(and (= 2020 (sum %)) %))
       product))

(def part1 #(solution 2 %))
(def part2 #(solution 3 %))
