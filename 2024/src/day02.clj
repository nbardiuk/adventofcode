(ns day02
  (:require
    [clojure.string :as str]))

(defn- read-lines [input]
  (for [line (str/split-lines input)]
    (mapv parse-long (re-seq #"\d+" line))))

(defn- safe? [xs]
  (let [diffs (mapv (fn [[a b]] (- a b)) (partition 2 1 xs))]
    (and (or (every? pos? diffs)
             (every? neg? diffs))
         (every? #(<= 1 (abs %) 3) diffs))))

(defn- drop-at [i xs]
  (concat (take i xs) (drop (inc i) xs)))

(defn part1 [input]
  (->> (read-lines input)
       (filter safe?)
       count))

(defn part2 [input]
  (->> (read-lines input)
       (filter (fn [xs]
                 (->> (range -1 (count xs))
                      (map #(drop-at % xs))
                      (some safe?))))
       count))
