(ns day09
  (:require [clojure.string :refer [split-lines]]))

(defn- read-numbers [input]
  (->> (split-lines input)
       (map read-string)))

(defn invalid-checksum [xs]
  (let [sum (last xs)
        xs (set (drop-last xs))]
    (when (not-any? #(xs (- sum %)) xs)
      sum)))

(defn invalid-number [preamble numbers]
  (->> numbers
       (partition (inc preamble) 1)
       (some invalid-checksum)))

(defn tails [xs]
  (->> (iterate rest xs)
       (take-while seq)))

(defn with-sum [sum xs]
  (let [sums (take-while #(<= % sum) (reductions + xs))]
    (when (= (last sums) sum)
      (take (count sums) xs))))

(defn part1 [input preamble]
  (->> (read-numbers input)
       (invalid-number preamble)))

(defn part2 [input preamble]
  (let [numbers (read-numbers input)
        invalid (invalid-number preamble numbers)]
    (->> (tails numbers)
         (some #(with-sum invalid %))
         (apply (juxt min max))
         (apply +))))
