(ns aoc.day01
  (:require [clojure.string :as cs]))

(defn- words [line] (cs/split line #", "))

(defn- parse-command [word]
  (let [[direction distance] (split-at 1 word)]
    {:direction (apply str direction)
     :distance (read-string (apply str distance))}))

(defn- parse-commands [line]
  (map parse-command (words line)))

(defn- navigate
  [{:keys [x y i j]}
   {:keys [direction distance]}]
  (let [[i j] (case direction
                "L" [(- j) i]
                "R" [j (- i)])
        x (+ x (* distance i))
        y (+ y (* distance j))]
    {:x x :y y :i i :j j}))

(defn- distance [{:keys [x y]}]
  (+ (Math/abs x) (Math/abs y)))

(defn part1 [input]
  (->> input
       parse-commands
       (reduce navigate {:x 0 :y 0 :i 0 :j -1})
       distance))

(defn- xrange [from to]
  (range from to (if (> from to) -1 1)))

(defn- trace [{from-x :x from-y :y} {to-x :x to-y :y}]
  (if (== from-y to-y)
    (map #(array-map :x %1 :y %2) (xrange from-x to-x) (repeat from-y))
    (map #(array-map :x %1 :y %2) (repeat from-x) (xrange from-y to-y))))

(defn- tracing [{:keys [path from]} to]
  {:path (concat path (trace from to))
   :from to})

(defn- contains [[x xs]] (xs x))

(defn- first-duplicate [items]
  (->> items
       (reductions conj #{})
       (map vector items)
       (some contains)))

(defn part2 [input]
  (->> input
       parse-commands
       (reductions navigate {:x 0 :y 0 :i 0 :j -1})
       (reduce tracing {:from {:x 0 :y 0}})
       :path
       first-duplicate
       distance))
