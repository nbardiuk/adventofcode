(ns day04)

(defn range-pairs
  [input]
  (->> (re-seq #"\d+" input)
       (map parse-long)
       (partition 2)
       (partition 2)))

(defn fully-contains?
  [[[a b] [x y]]]
  (or (<= a x y b)
      (<= x a b y)))

(defn partially-overlaps?
  [[[a b] [x y]]]
  (or (<= x a y)
      (<= a x b)))

(defn part1
  [input]
  (->> (range-pairs input)
       (filter fully-contains?)
       count))

(defn part2
  [input]
  (->> (range-pairs input)
       (filter partially-overlaps?)
       count))
