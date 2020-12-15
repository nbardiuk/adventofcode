(ns day15)

(defn read-numbers [input]
  (->> (re-seq #"\d+" input)
       (map read-string)))

(defn init-state [xs]
  [(last xs)
   (count xs)
   (->> xs
        (map-indexed (fn [i n] [n [i i]]))
        (into {}))])

(defn step [[last index seen]]
  (let [[a b] (get seen last [0 0])
        next (- a b)
        seen (update seen next (fn [[x]] [index (or x index)]))]
    [next (inc index) seen]))

(defn solution [n input]
  (let [[_ index :as state] (->> (read-numbers input) init-state)]
    (-> (iterate step state)
        (nth (- n index))
        first)))

(defn part1 [input] (solution 2020 input))
(defn part2 [input] (solution 30000000 input))
