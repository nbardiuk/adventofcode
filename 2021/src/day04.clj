(ns day04
  (:require [clojure.string :as string]))

(defn- parse-input [input]
  (let [[draws & boards] (string/split-lines input)]
    {:draws (->> draws (re-seq #"\d+") (map parse-long))
     :boards (->> boards
                  (mapcat #(re-seq #"\d+" %))
                  (map (fn [value] {:marked? false :number (parse-long value)}))
                  (partition 25))}))

(defn- bingo? [board]
  (let [rows (->> board (partition 5))
        cols (apply map vector rows)]
    (->> (concat rows cols)
         (some #(every? :marked? %))
         some?)))

(defn- mark [board draw]
  (for [cell board]
    (update cell :marked? #(or % (= draw (:number cell))))))

(defn- score [board draw]
  (* draw
     (->> board (remove :marked?) (map :number) (reduce + 0))))

(defn- lazy-scores [boards [draw & next-draws]]
  (lazy-seq
   (when draw
     (let [marked (map #(mark % draw) boards)
           {finished true playing false} (group-by bingo? marked)]
       (concat (map #(score % draw) finished)
               (lazy-scores playing next-draws))))))

(defn- scores [input]
  (let [{:keys [boards draws]} (parse-input input)]
    (lazy-scores boards draws)))

(def part1 (comp first scores))
(def part2 (comp last scores))
