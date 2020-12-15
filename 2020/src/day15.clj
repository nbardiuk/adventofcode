(ns day15
  (:import [java.util HashMap]))

(defn read-numbers [input]
  (->> (re-seq #"\d+" input)
       (map read-string)))

(defn init-state [xs]
  [0
   (count xs)
   (->> xs (map-indexed (fn [i n] [n i])) (into {}) (HashMap.))])

(defn step [[item i ^HashMap seen]]
  (let [j (.put seen item i)
        next (- i (or j i))]
    [next (inc i) seen]))

(defn solution [n input]
  (let [[_ i :as state] (->> (read-numbers input) init-state)]
    (-> (iterate step state)
        (nth (dec (- n i)))
        first)))

(defn part1 [input] (solution 2020 input))
(defn part2 [input] (solution 30000000 input))
