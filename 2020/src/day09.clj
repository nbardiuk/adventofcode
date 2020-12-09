(ns day09
  (:require [clojure.string :refer [split-lines]]))

(defn- read-numbers [input]
  (->> (split-lines input)
       (map read-string)))

(defn sum2-last [xs]
  (let [sum (last xs)
        xs (set (drop-last xs))]
    (when (not-any? #(xs (- sum %)) xs)
      sum)))

(defn invalid-number [preamble numbers]
  (->> numbers
       (partition (inc preamble) 1)
       (some sum2-last)))

(defn tails [xs]
  (->> (iterate rest xs)
       (take-while seq)))

(defn take-with-sum [sum xs]
  (let [sums (take-while #(<= % sum) (reductions + xs))
        len (if (= (last sums) sum) (count sums) 0)]
    (take len xs)))

(defn part1 [input preamble]
  (->> (read-numbers input)
       (invalid-number preamble)))

(defn part2 [input preamble]
  (let [numbers (read-numbers input)
        invalid (invalid-number preamble numbers)]
    (->> (tails numbers)
         (map #(take-with-sum invalid %))
         (apply max-key count)
         (apply (juxt min max))
         (apply +))))
