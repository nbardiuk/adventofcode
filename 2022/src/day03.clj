(ns day03
  (:require
   [clojure.set :as set]
   [clojure.string :as str]))

(defn priority [char]
  (let [code  (int char)
        a     (int \a)
        A     (int \A)
        shift (if (<= a code) (- a 1) (- A 27))]
    (- code shift)))

(defn halves [xs]
  (let [midle (/ (count xs) 2)]
    (split-at midle xs)))

(defn shared-item [group]
  (->> group
       (map set)
       (reduce set/intersection)
       first))

(defn sum-of-groups-priorities
  [input into-groups]
  (->> (str/split-lines input)
       into-groups
       (map shared-item)
       (map priority)
       (reduce +)))

(defn part1 [input]
  (sum-of-groups-priorities input #(map halves %)))

(defn part2 [input]
  (sum-of-groups-priorities input #(partition 3 %)))
