(ns day24
  (:require [clojure.string :as str]))

(def directions
  {"e"  [1  0]
   "w"  [-1 0]
   "ne" [1 -1]
   "nw" [0 -1]
   "se" [0  1]
   "sw" [-1 1]})

(defn parse-tiles [input]
  (->> (for [line (str/split-lines input)]
         (->> (re-seq #"e|w|se|sw|ne|nw" line)
              (map directions)
              (reduce #(mapv + %1 %2))))
       (reduce (fn [black tile]
                 (if (black tile)
                   (disj black tile)
                   (conj black tile)))
               #{})))

(defn neigbours [tile]
  (->> (vals directions)
       (map #(mapv + tile %))))

(defn step [black-tiles]
  (set
   (for [[tile n] (frequencies (mapcat #(neigbours %) black-tiles))
         :when (if (black-tiles tile)
                 (not (or (zero? n) (< 2 n)))
                 (= 2 n))]
     tile)))

(defn solution [steps input]
  (count (nth (iterate step (parse-tiles input)) steps)))

(def part1 (partial solution 0))
(def part2 (partial solution 100))
