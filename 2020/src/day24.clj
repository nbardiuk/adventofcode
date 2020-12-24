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

(defn neighbors [tile]
  (->> (vals directions)
       (map #(mapv + tile %))))

(defn step [black]
  (set
   (for [[tile n] (frequencies (mapcat #(neighbors %) black))
         :when (if (black tile)
                 (#{1 2} n)
                 (= 2 n))]
     tile)))

(defn solution [steps input]
  (-> (iterate step (parse-tiles input))
      (nth steps)
      (count)))

(def part1 (partial solution 0))
(def part2 (partial solution 100))
