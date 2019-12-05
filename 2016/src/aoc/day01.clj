(ns aoc.day01
  (:require [clojure.string :as cs]
            ))

(defn words [line] (cs/split line #", "))

(defn parse-command [word]
  (let [[direction distance] (split-at 1 word)]
    [(apply str direction ) (read-string (apply str distance ))]))

(defn parse-commands [line] 
  (map parse-command (words line)))

(defn navigate [[[x y] orientation] [direction distance]] 
  (case [orientation direction]
    ["N" "R"] [[(+ x distance) y] , "E"]
    ["N" "L"] [[(- x distance) y] , "W"]
    ["E" "R"] [[x (+ y distance)] , "S"]
    ["E" "L"] [[x (- y distance)] , "N"]
    ["S" "R"] [[(- x distance) y] , "W"]
    ["S" "L"] [[(+ x distance) y] , "E"]
    ["W" "R"] [[x (- y distance)] , "N"]
    ["W" "L"] [[x (+ y distance)] , "S"]))

(defn distance [position]
  (let [[x y] position]
    (+ (Math/abs x) (Math/abs y))))

(defn part1 [input]
  (let [commands (parse-commands input)
        [position] (reduce navigate [[0 0] "N"] commands)]
    (distance position)))

(defn xrange [from to] (range from to (if (> from to) -1 1)))

(defn track [[result [from-x from-y]] [to-x to-y]]
  (let [trace (if (== from-y to-y)
                (map #(-> [% from-y]) (xrange from-x to-x))
                (map #(-> [from-x %]) (xrange from-y to-y)))]
    [(concat result trace) [to-x to-y]]))

(defn contains [[ x xs ]] (some #(= x %) xs))

(defn first-duplicate [items]
  (let [inits (reductions conj '() items)
        with-inits (map vector items inits)
        [result] (first (filter contains with-inits))]
    result))

(defn part2 [input]
  (let [commands (parse-commands input)
        navigation (reductions navigate [[0 0] "N"] commands)
        turns (map first navigation)
        [traces] (reduce track ['() [0 0]] turns)]
    (distance (first-duplicate traces))))
