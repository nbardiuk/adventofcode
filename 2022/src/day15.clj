(ns day15
  (:require [clojure.set :as set]))

(defn parse-longs [input]
  (->> (re-seq #"-?\d+" input)
       (map parse-long)))

(defn distance [[ax ay] [bx by]]
  (+ (abs (- ax bx))
     (abs (- ay by))))

(defn read-input [input]
  (->> (parse-longs input)
       (partition 2)
       (partition 2)
       (map (fn [[s b]] [s b (distance s b)]))))

(defn first-gap [ranges]
  (loop [[[a b] [c d] & ranges] (sort ranges)]
    (cond
      (nil? c) nil
      (< b c)  b
      :else   (recur (cons [a (max b d)] ranges)))))

(defn part1 [row input]
  (let [data (read-input input)]
    (->> (for [[[x y] beacon dist] data
               :let [d (- dist (abs (- y row)))]
               :when (< 0 d)]
           (set (for [x (range (- x d) (inc (+ x d)))
                      :when (not= [x row] beacon)]
                  x)))
         (reduce set/union)
         count)))

(defn part2 [mag input]
  (let [data (read-input input)]
    (first
     (for [row (range (inc mag))
           :let [x (first-gap
                    (for [[[x y] _ dist] data
                          :let [d (- dist (abs (- y row)))]
                          :when (< 0 d)]
                      [(max 0 (- x d)) (inc (min mag (+ x d)))]))]
           :when x]
       (+ (* 4000000 x) row)))))
