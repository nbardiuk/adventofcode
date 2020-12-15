(ns day15
  (:import [java.util HashMap]))

(defn read-numbers [input]
  (->> (re-seq #"\d+" input)
       (map read-string)))

(defn solution [n input]
  (let [xs (read-numbers input)
        seen (->> xs (map-indexed (fn [i n] [n i])) (into {}) (HashMap.))]
    (loop [item 0
           i (count xs)]
      (cond
        (= i (dec n)) item
        :else (recur (- i (or (.put seen item i) i))
                     (inc i))))))

(defn part1 [input] (solution 2020 input))
(defn part2 [input] (solution 30000000 input))
