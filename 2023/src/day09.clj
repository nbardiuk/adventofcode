(ns day09
  (:require
    [clojure.string :as str]))


(defn parse-readings [input]
  (->> (str/split-lines input)
       (mapv #(->> % (re-seq #"-?\d+") (mapv parse-long)))))


(defn diff [xs]
  (->> (partition 2 1 xs)
       (mapv (fn [[a b]] (- b a)))))


(defn diffs-triangle [xs]
  (->> xs
       (iterate diff)
       (take-while #(not (every? zero? %)))
       vec))


(defn solution [input extrapolate]
  (->> (parse-readings input)
       (mapv #(reduce extrapolate 0 (reverse (diffs-triangle %))))
       (reduce +)))


(defn part1 [input]
  (solution input (fn [prev-val diffs]
                    (+ (last diffs) prev-val))))


(defn part2 [input]
  (solution input (fn [prev-val diffs]
                    (- (first diffs) prev-val))))
