(ns day17
  (:require [clojure.string :as str]))

(defn read-alive [origin input]
  (let [lines (str/split-lines input)
        size (count lines)]
    (for [x (range size)
          y (range size)
          :when (= \# (get-in lines [y x]))]
      (-> origin (assoc 0 x) (assoc 1 y)))))

(defn directions [dimentions]
  (->> (loop [n dimentions
              result [[]]]
         (case n
           0 result
           (recur (dec n)
                  (mapcat (fn [x] (map #(conj % x) result)) [-1 0 1]))))
       (remove #(apply = 0 %))
       vec))

(defn neighbours [directions cube]
  (map #(mapv + cube %) directions))

(defn step [directions active]
  (set
   (for [cube (distinct (mapcat (partial neighbours directions) active))
         :let [n (->> cube (neighbours directions) (filter active) count)]
         :when (if (active cube)
                 (#{2 3} n)
                 (#{3} n))]
     cube)))

(defn solution [dimentions input]
  (let [origin (vec (repeat dimentions 0))
        directions (directions dimentions)
        active (->> input (read-alive origin) set)]
    (-> (iterate #(step directions %) active)
        (nth 6)
        count)))

(defn part1 [input] (solution 3 input))
(defn part2 [input] (solution 4 input))
