(ns day09
  (:require [clojure.string :refer [split-lines]]))

(defn- read-numbers [input]
  (->> (split-lines input)
       (map read-string)))

(defn has-sum [xs]
  (let [x (last xs)
        xs (set (drop-last xs))]
    (when (not-any? #(xs (- x %)) xs)
      x)))

(defn invalid-number [preamble numbers]
  (->> numbers
       (partition (inc preamble) 1)
       (some has-sum)))

(defn tails [xs]
  (->> xs
       (iterate rest)
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
        sum (invalid-number preamble numbers)]
    (->> (tails numbers)
         (keep #(with-sum sum %))
         (apply max-key count)
         (apply (juxt min max))
         (apply +))))
