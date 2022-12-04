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

(defn count-overlaping-ranges
  [overlaps? input]
  (->> (range-pairs input)
       (filter overlaps?)
       count))

(defn part1
  [input]
  (count-overlaping-ranges fully-contains? input))

(defn part2
  [input]
  (count-overlaping-ranges partially-overlaps? input))
