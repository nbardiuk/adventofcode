(ns aoc.day02
  (:require [clojure.string :as s]))

(def simple-keypad
  [[\1 \2 \3]
   [\4 \5 \6]
   [\7 \8 \9]])

(def complex-keypad
  [[nil nil \1 nil nil]
   [nil \2  \3 \4  nil]
   [\5  \6  \7 \8  \9]
   [nil \A  \B \C  nil]
   [nil nil \D nil nil]])

(defn- label [keypad {:keys [x y]}]
  (-> keypad (get y) (get x)))

(defn- button [keypad position]
  (some->> position
           (label keypad)
           (assoc {:position position} :label)))

(defn- neighbour [{:keys [x y]} direction]
  (case direction
    \U {:x x       :y (- y 1)}
    \D {:x x       :y (+ y 1)}
    \L {:x (- x 1) :y y}
    \R {:x (+ x 1) :y y}))

(defn- move [keypad {:keys [position] :as current} direction]
  (or (button keypad (neighbour position direction))
      current))

(defn- moves [keypad button instruction]
  (reduce (partial move keypad) button instruction))

(defn- buttons [keypad start instructions]
  (reductions (partial moves keypad) (button keypad start) instructions))

(defn- decode [input keypad start]
  (->> input
       s/split-lines
       (buttons keypad start)
       (map :label)
       (drop 1)
       (apply str)))

(defn part1 [input]
  (decode input simple-keypad {:x 1 :y 1}))

(defn part2 [input]
  (decode input complex-keypad {:x 0 :y 2}))
