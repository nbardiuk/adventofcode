(ns day11
  (:require [clojure.string :refer [split-lines]]))

(defn- parse-map [input]
  (let [lines (split-lines input)
        [width height] [(count (first lines)) (count lines)]
        positions (group-by (fn [[x y]] (get-in lines [y x]))
                            (for [x (range width) y (range height)] [x y]))]
    {:occupied? #{}
     :floor? (set (get positions \.))
     :seats (get positions \L)}))

(defn- neighbour [_floor? [x y] [dx dy]]
  [(+ x dx) (+ y dy)])

(defn- first-seen [floor? p [dx dy]]
  (->> (iterate inc 1)
       (map #(neighbour floor? p [(* % dx) (* % dy)]))
       (drop-while floor?)
       first))

(defn- precompute-neighbours [seat-lookup {:keys [seats floor?] :as m}]
  (let [directions [[-1 -1] [-1 0] [-1 1]
                    [0  -1]        [0  1]
                    [1  -1] [1  0] [1  1]]]
    (->> seats
         (map #(vector % (map (partial seat-lookup floor? %) directions)))
         (into {})
         (assoc m :neighbours))))

(defn- steps [tolerance {:keys [occupied? seats neighbours]}]
  (->> occupied?
       (iterate
        (fn [occupied?]
          (->> seats
               (filter
                #(if (occupied? %)
                   (not= tolerance (->> (neighbours %) (filter occupied?) (bounded-count tolerance)))
                   (not-any? occupied? (neighbours %))))
               set)))))

(defn- first-repeat [xs]
  (->> (partition 2 1 xs)
       (drop-while #(apply not= %))
       ffirst))

(defn- stable-occupation [input seat-lookup tolerance]
  (->> (parse-map input)
       (precompute-neighbours seat-lookup)
       (steps tolerance)
       first-repeat
       count))

(defn part1 [input]
  (stable-occupation input neighbour 4))

(defn part2 [input]
  (stable-occupation input first-seen 5))
