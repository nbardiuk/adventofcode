(ns day05
  (:require [clojure.string :as str]))

(defn parse-longs [input]
  (->> (re-seq #"\d+" input)
       (map parse-long)))

(defn transpose [rows]
  (let [columns (->> rows (map count) (reduce max))]
    (for [i (range columns)]
      (keep #(nth % i nil) rows))))

(defn find-first [p xs]
  (first (filter p xs)))

(defn parse-commands [input]
  (for [[move from to] (partition 3 (parse-longs input))]
    {:move  move
     :from (dec from)
     :to   (dec to)}))

(defn parse-stacks [input]
  (->> (for [line (butlast (str/split-lines input))]
         (for [crate (partition-all 4 line)]
           (find-first #(Character/isLetter %) crate)))
       transpose
       vec))

(defn parse-input [input]
  (let [[stacks commands] (str/split input #"\R\R")]
    {:stacks   (parse-stacks stacks)
     :commands (parse-commands commands)}))

(defn step
  [moving-order stacks {:keys [move from to]}]
  (let [moving (take move (get stacks from))]
    (-> stacks
        (update from #(drop move %))
        (update to   #(concat (moving-order moving) %)))))

(defn top-crates [moving-order input]
  (let [{:keys [stacks commands]} (parse-input input)
        stacks (reduce (partial step moving-order) stacks commands)]
    (str/join (map first stacks))))

(defn part1 [input]
  (top-crates reverse input))

(defn part2 [input]
  (top-crates identity input))
