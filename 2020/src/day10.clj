(ns day10
  (:require [clojure.string :refer [split-lines]]))

(defn- read-numbers [input]
  (->> (split-lines input)
       (map read-string)))

(defn- with-edges [ns]
  (conj ns 0 (+ (apply max ns) 3)))

(defn count-connections [ns]
  (let [n (count ns)]
    (/ (+ (* n n) (* -3 n) 4) 2)))

(defn connected-components [ns]
  (let [prev (atom 0)
        group (atom true)]
    (->> (sort ns)
         (partition-by
          (fn [n]
            (when (= 3 (- n @prev))
              (swap! group not))
            (reset! prev n)
            @group)))))

(defn part1 [input]
  (->> (read-numbers input)
       with-edges
       sort
       (partition 2 1)
       (map (fn [[a b]] (- b a)))
       frequencies
       vals
       (apply *)))

(defn part2 [input]
  (->> (read-numbers input)
       with-edges
       connected-components
       (map count-connections)
       (apply *)))
