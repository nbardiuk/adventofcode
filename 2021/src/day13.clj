(ns day13
  (:require [clojure.string :as string]))

(defn parse-input [input]
  {:dots
   (for [[_ x y] (re-seq #"(\d+),(\d+)" input)]
     [(parse-long x) (parse-long y)])

   :instructions
   (for [[_ axis line] (re-seq #"(x|y)=(\d+)" input)]
     [({"x" 0 "y" 1} axis) (parse-long line)])})

(defn fold-one [v line]
  (if (<= v line)
    v
    (- line (- v line))))

(defn fold [dots [axis line]]
  (->> dots
       (mapv #(update % axis fold-one line))
       set))

(defn part1 [input]
  (let [{:keys [dots instructions]} (parse-input input)]
    (->> (fold dots (first instructions))
         count)))

(defn part2 [input]
  (let [{:keys [dots instructions]} (parse-input input)
        dots (reduce fold dots instructions)
        max-y (->> dots (map second) (reduce max))
        max-x (->> dots (map first) (reduce max))]
    (->> (for [y (range (inc max-y))]
           (for [x (range (inc max-x))]
             (if (dots [x y]) "#" " ")))
         (map string/join)
         (string/join "\n"))))
