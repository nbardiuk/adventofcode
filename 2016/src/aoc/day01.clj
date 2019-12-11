(ns aoc.day01
  (:require [clojure.string :as cs]))

(defn words [line] (cs/split line #", "))

(defn parse-command [word]
  (let [[direction distance] (split-at 1 word)]
    [(apply str direction) (read-string (apply str distance))]))

(defn parse-commands [line] 
  (map parse-command (words line)))

(defn navigate [[[x y] [i j]] [direction distance]] 
  (let [[i j] (case direction
                "L" [(- j) i]
                "R" [j (- i)])
        x (+ x (* distance i))
        y (+ y (* distance j))]
    [[x y] [i j]]))

(defn distance [[x y]]
  (+ (Math/abs x) (Math/abs y)))

(defn part1 [input]
  (let [commands (parse-commands input)
        [position] (reduce navigate [[0 0] [0 -1]] commands)]
    (distance position)))

(defn xrange [from to] (range from to (if (> from to) -1 1)))

(defn track [[result [from-x from-y]] [to-x to-y]]
  (let [trace (if (== from-y to-y)
                (map #(-> [% from-y]) (xrange from-x to-x))
                (map #(-> [from-x %]) (xrange from-y to-y)))]
    [(concat result trace) [to-x to-y]]))

(defn seen? [[ x xs ]] (contains? xs x))

(defn first-duplicate [items]
  (let [inits (reductions conj #{} items)
        with-inits (map vector items inits)
        [result] (first (filter seen? with-inits))]
    result))

(defn part2 [input]
  (let [commands (parse-commands input)
        navigation (reductions navigate [[0 0] [0 -1]] commands)
        turns (map first navigation)
        [traces] (reduce track ['() [0 0]] turns)]
    (distance (first-duplicate traces))))
