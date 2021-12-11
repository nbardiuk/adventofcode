(ns day11
  (:require [clojure.string :as string]))

(defn parse-grid [input]
  (let [lines (string/split-lines input)]
    (->> (for [y (range (count lines))
               :let [line (get lines y)]
               x (range (count line))
               :let [level (subs line x (inc x))]]
           {[x y] (parse-long level)})
         (apply merge))))

(defn fix-point [f x]
  (->> (iterate f x)
       (partition 2 1)
       (drop-while #(apply not= %))
       ffirst))

(defn neighbours [[x y]]
  (for [dx [-1 0 1]
        dy [-1 0 1]
        :let [x' (+ x dx)
              y' (+ y dy)]
        :when (and (<= 0 x' 9) (<= 0 y' 9)
                   (not= [x y] [x' y']))]
    [x' y']))

(defn flash [levels]
  (reduce
   (fn [levels pos]
     (if (< 9 (get levels pos))
       (->> (neighbours pos)
            (remove #(zero? (get levels %)))
            (reduce #(update %1 %2 inc) (assoc levels pos 0)))
       levels))
   levels (keys levels)))

(defn step [levels]
  (->> (update-vals levels inc)
       (fix-point flash)))

(defn index-of [pred coll]
  (->> coll
       (keep-indexed #(when (pred %2) %1))
       first))

(defn part1 [input]
  (->> (parse-grid input)
       (iterate step)
       (take 101)
       (mapcat #(filter zero? (vals %)))
       count))

(defn part2 [input]
  (->> (parse-grid input)
       (iterate step)
       (index-of #(every? zero? (vals %)))))
