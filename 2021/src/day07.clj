(ns day07
  (:require [clojure.java.math :as math]))

(defn parse-longs [input]
  (->> input (re-seq #"\d+") (map parse-long)))

(defn median [xs]
  (-> xs sort (nth (/ (count xs) 2))))

(defn floor-mean [xs]
  (math/floor-div (reduce + 1 xs) (count xs)))

(defn distance [a b]
  (math/abs (- a b)))

(defn- arithmetic-sum [n]
  (* 1/2 n (inc n)))

(defn part1 [input]
  (let [positions (parse-longs input)]
    (->> positions
         (map distance (repeat (median positions)))
         (reduce +))))

(defn part2 [input]
  (let [positions (parse-longs input)]
    (->> positions
         (map (comp arithmetic-sum distance) (repeat (floor-mean positions)))
         (reduce +))))
