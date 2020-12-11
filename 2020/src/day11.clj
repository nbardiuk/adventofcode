(ns day11
  (:require [clojure.string :refer [split-lines]]))

(defn- parse-map [input]
  (into {} (let [lines (split-lines input)]
             (for [y (range (count lines))
                   x (range (count (first lines)))]
               [[x y] (get-in lines [y x])]))))

(def directions [[-1 -1]
                 [-1 1]
                 [-1 0]
                 [0 -1]
                 [0 1]
                 [1 0]
                 [1 1]
                 [1 -1]])

(defn- neighbour [m [x y] [dx dy]]
  (get m [(+ x dx) (+ y dy)]))

(defn- first-seen [m pos [dx dy]]
  (->> (iterate inc 1)
       (map #(vector (* % dx) (* % dy)))
       (map #(neighbour m pos %))
       (drop-while #{\.})
       first))

(defn- count-occupied [seat-lookup m pos]
  (count (for [direction directions
               :when (= \# (seat-lookup m pos direction))]
           1)))

(defn- step [seat-lookup tolerance m]
  (reduce
   (fn [result [pos v]]
     (cond
       (and (= \L v) (= 0 (count-occupied seat-lookup m pos))) (assoc result pos \#)
       (and (= \# v) (<= tolerance (count-occupied seat-lookup m pos))) (assoc result pos \L)
       :else result))
   m
   m))

(defn- first-repeat [xs]
  (->> (partition 2 1 xs)
       (drop-while #(apply not= %))
       ffirst))

(defn- occupation [m]
  (->> (vals m)
       (filter #{\#})
       count))

(defn- stable-occupation [input seat-lookup tolerance]
  (->> (parse-map input)
       (iterate (partial step seat-lookup tolerance))
       first-repeat
       occupation))

(defn part1 [input]
  (stable-occupation input neighbour 4))

(defn part2 [input]
  (stable-occupation input first-seen 5))
