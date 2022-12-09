(ns day09
  (:require [clojure.string :as str]))

(defn parse-motions [input]
  (for [line (str/split-lines input)]
    (let [[dir steps] (str/split line #"\s")]
      [(keyword dir) (parse-long steps)])))

(defn signum [n]
  (long (Math/signum (double n))))

(defn follow [head {:keys [x y] :as tail}]
  (let [dx (- (:x head) x)
        dy (- (:y head) y)
        touching? (and (<= (abs dx) 1)
                       (<= (abs dy) 1))]
    (if touching?
      tail
      {:x (+ x (signum dx))
       :y (+ y (signum dy))})))

(defn move-knot [knot direction]
  (case direction
    :L (update knot :x dec)
    :R (update knot :x inc)
    :U (update knot :y dec)
    :D (update knot :y inc)))

(defn move-rope [[head & rest] direction]
  (reduce
   (fn [rope tail]
     (conj rope (follow (peek rope) tail)))
   [(move-knot head direction)]
   rest))

(defn tail-track-size [rope-length input]
  (let [rope (repeat rope-length {:x 0 :y 0})]
    (->> (parse-motions input)
         (mapcat (fn [[direction steps]]
                   (repeat steps direction)))
         (reductions move-rope rope)
         (map last)
         distinct
         count)))

(defn part1 [input]
  (tail-track-size 2 input))

(defn part2 [input]
  (tail-track-size 10 input))
