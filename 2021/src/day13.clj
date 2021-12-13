(ns day13
  (:require [clojure.string :as string]
            [clojure.java.math :as math]))

(defn parse-input [input]
  {:dots
   (for [[_ x y] (re-seq #"(\d+),(\d+)" input)]
     [(parse-long x) (parse-long y)])

   :instructions
   (for [[_ axis line] (re-seq #"(x|y)=(\d+)" input)]
     [({"x" 0 "y" 1} axis) (parse-long line)])})

(defn fold-one [value line]
  (- line (math/abs (- line value))))

(defn fold [dots [axis line]]
  (set (map #(update % axis fold-one line) dots)))

(defn part1 [input]
  (let [{:keys [dots instructions]} (parse-input input)]
    (count (fold dots (first instructions)))))

(defn part2 [input]
  (let [{:keys [dots instructions]} (parse-input input)
        dots (reduce fold dots instructions)]
    (->> (for [y (range (inc (apply max (map second dots))))]
           (for [x (range (inc (apply max (map first dots))))]
             (if (dots [x y]) "#" " ")))
         (map string/join)
         (string/join "\n"))))
