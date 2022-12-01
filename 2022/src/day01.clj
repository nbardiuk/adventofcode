(ns day01
  (:require [clojure.string :as str]))

(defn- parse-longs [input]
  (->> input (re-seq #"\d+") (map parse-long)))

(defn parse-inventory [input]
  (->> (str/split input #"\R\R")
       (map parse-longs)))

(defn- sum [xs]
  (reduce + 0 xs))

(defn- top [n xs]
  (->> xs (sort >) (take n)))

(defn- sum-top-carriers [input n]
  (->> (parse-inventory input)
       (map sum)
       (top n)
       sum))

(defn part1 [input]
  (sum-top-carriers input 1))

(defn part2 [input]
  (sum-top-carriers input 3))

