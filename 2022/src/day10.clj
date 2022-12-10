(ns day10
  (:require [clojure.string :as str]))

(defn parse-program [input]
  (for [line (str/split-lines input)]
    (let [[command arg] (str/split line #"\s")]
      [(keyword command) (when arg (parse-long arg))])))

(defn time-sync [[command :as instruction]]
  (let [padding (if (= :addx command) [[:noop]] [])]
    (conj padding instruction)))

(defn eval-x [x [_command arg]]
  (+ x (or arg 0)))

(defn signal [input]
  (->> (parse-program input)
       (mapcat time-sync)
       (reductions eval-x 1)
       vec))

(defn part1 [input]
  (let [signal (signal input)]
    (->> (for [time (range 20 (inc 220) 40)]
           (* time (get signal (dec time))))
         (reduce +))))

(defn part2 [input]
  (let [signal (signal input)]
    (->> (for [y (range 6)]
           (for [x (range 40)]
             (let [time     (+ x (* 40 y))
                   sprite   (get signal time)
                   overlap? (<= -1 (- x sprite) 1)]
               (if overlap? "#" "."))))
         (map str/join))))
