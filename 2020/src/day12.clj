(ns day12
  (:require [clojure.string :as str]))

(defn read-commands [input]
  (for [line (str/split-lines input)
        :let [[_ cmd n] (re-find #"(\w)(\d+)" line)]]
    [cmd (read-string n)]))

(defn rotate [[x y] dg]
  (let [[a b c d] (case dg
                    90  [0  1
                         -1 0]
                    180 [-1 0
                         0 -1]
                    270 [0 -1
                         1  0])]
    [(+ (* x a) (* y b))
     (+ (* x c) (* y d))]))

(defn move [[sx sy :as shift]
            {:keys [x y dx dy] :as ship}
            [cmd n]]
  (case cmd
    "F" (assoc ship :x (+ x (* n dx)) :y (+ y (* n dy)))
    "E" (update ship sx + n)
    "N" (update ship sy + n)
    "R" (let [[dx dy] (rotate [dx dy] n)] (assoc ship :dx dx :dy dy))
    "W" (move shift ship ["E" (- 0 n)])
    "S" (move shift ship ["N" (- 0 n)])
    "L" (move shift ship ["R" (- 360 n)])))

(defn distance [movement [dx dy] input]
  (let [{:keys [x y]} (->> (read-commands input)
                           (reduce movement {:x 0 :y 0 :dx dx :dy dy}))]
    (+ (Math/abs x) (Math/abs y))))

(defn part1 [input] (distance (partial move [:x :y]) [1 0] input))
(defn part2 [input] (distance (partial move [:dx :dy]) [10 1] input))
