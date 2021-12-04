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
         (some #(= (repeat 5 true) (map :marked? %)))
         some?)))

(defn- mark [board draw]
  (for [cell board]
    (update cell :marked? #(or % (= draw (:number cell))))))

(defn- score [board draw]
  (* draw
     (->> board (remove :marked?) (map :number) (reduce + 0))))

(defn- scores [input]
  (let [{:keys [draws boards]} (parse-input input)]
    ((fn lazy-scores [[draw & next-draws] boards]
       (lazy-seq
        (when draw
          (let [{bingo-boards true next-boards false}
                (->> boards (map #(mark % draw)) (group-by bingo?))]
            (concat (map #(score % draw) bingo-boards)
                    (lazy-scores next-draws next-boards))))))
     draws boards)))

(def part1 (comp first scores))
(def part2 (comp last scores))
