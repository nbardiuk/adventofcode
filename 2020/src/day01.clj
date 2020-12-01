(ns day01
  (:require [clojure.string :refer [split-lines]]))

(defn- sorted-numbers [input]
  (->> input
       split-lines
       (map #(Integer. %))
       (sort <)))

(defn tails [xs]
  (->> xs
       (iterate rest)
       (take-while seq)))

(defn part1 [input]
  (first
   (for [[x & ys] (tails (sorted-numbers input))
         y ys
         :when (= 2020 (+ x y))]
     (* x y))))

(defn part2 [input]
  (first
   (for [[x & ys] (tails (sorted-numbers input))
         [y & zs] (tails ys)
         z zs
         :when (= 2020 (+ x y z))]
     (* x y z))))
