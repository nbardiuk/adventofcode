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

(defn boundary [mag [x y] dist]
  (for [d (range (+ 2 dist))
        x [(- x d) (+ x d)]
        :when (<= 0 x mag)
        :let [d (- (inc dist) d)]
        y [(- y d) (+ y d)]
        :when (<= 0 y mag)]
    [x y]))

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
  (let [data (sort-by ffirst > (read-input input))]
    (first
     (for [p     (->> data (mapcat (fn [[s _ d]] (boundary mag s d))))
           :when (->> data (every? (fn [[s _ d]] (< d (distance p s)))))
           :let  [[x y] p]]
       (+ (* 4000000 x) y)))))
