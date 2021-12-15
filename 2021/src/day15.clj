(ns day15
  (:require [clojure.string :as string])
  (:import [java.util PriorityQueue]))

(defn parse-input [input]
  (let [lines (string/split-lines input)]
    (->> (for [y (range (count lines))
               :let [line (nth lines y)]
               x (range (count line))]
           [[x y] (parse-long (subs line x (inc x)))])
         (into {}))))

(defn neighbours [[x y]]
  [[x (dec y)]
   [(dec x) y] [(inc x) y]
   [x (inc y)]])

(defn grid-size [grid]
  (let [max-by #(->> grid keys (map %) (apply max) inc)]
    [(max-by first) (max-by second)]))

(defn tile [times grid]
  (let [[w h] (grid-size grid)]
    (->> (for [[[ox oy] ov] grid
               i (range times)
               j (range times)
               :let [x (+ ox (* i w))
                     y (+ oy (* j h))
                     v (-> (iterate #(inc (mod % 9)) ov)
                           (nth (+ i j)))]]
           [[x y] v])
         (into {}))))

(defn total-risk [grid]
  (let [[w h] (grid-size grid)
        dest [(dec w) (dec h)]
        start [0 0]
        queue (new PriorityQueue [[0 start]])]
    (loop [grid (dissoc grid start)]
      (let [[risk pos] (.poll queue)]
        (if (= pos dest)
          risk
          (let [elements (for [npos (neighbours pos)
                               :let [nrisk (grid npos)]
                               :when nrisk]
                           [(+ risk nrisk) npos])]
            (.addAll queue elements)
            (recur (->> elements (map second) (reduce dissoc grid)))))))))

(defn solution [tiling input]
  (total-risk (tile tiling (parse-input input))))

(def part1 (partial solution 1))
(def part2 (partial solution 5))
