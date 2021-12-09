(ns day09
  (:require [clojure.string :as string]))

(defn parse-map [input]
  (vec
   (for [line (string/split-lines input)]
     (->> line (re-seq #"\d") (mapv parse-long)))))

(defn grid [heights]
  (for [x (range (count (first heights)))
        y (range (count heights))]
    [y x]))

(defn- neighbours [[y x]]
  [[(dec y) x]
   [y (dec x)] [y (inc x)]
   [(inc y) x]])

(defn part1 [input]
  (let [heights (parse-map input)]
    (->> (for [position (grid heights)
               :let [h (get-in heights position)]
               :when (every? #(< h (get-in heights % 10)) (neighbours position))]
           (inc h))
         (reduce +))))

(defn- basin-sizes [heights]
  (lazy-seq
   (when-let [start (->> heights grid (filter #(< (get-in heights %) 9)) first)]
     (loop [[position & queue] [start]
            heights (assoc-in heights start 10)
            size 1]
       (if position
         (let [neighbours (filter #(< (get-in heights % 10) 9) (neighbours position))]
           (recur (concat queue neighbours)
                  (reduce #(assoc-in %1 %2 10) heights neighbours)
                  (+ size (count neighbours))))
         (cons size (basin-sizes heights)))))))

(defn part2 [input]
  (->> input
       parse-map
       basin-sizes
       (sort >)
       (take 3)
       (reduce *)))
