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

(defn- neighbor [_floor? [x y] [dx dy]]
  [(+ x dx) (+ y dy)])

(defn- first-seen [floor? p [dx dy]]
  (->> (iterate inc 1)
       (map #(neighbor floor? p [(* % dx) (* % dy)]))
       (drop-while floor?)
       first))

(defn- precompute-neighbors [seat-lookup {:keys [seats floor?] :as m}]
  (let [directions [[-1 -1] [-1 0] [-1 1]
                    [0  -1]        [0  1]
                    [1  -1] [1  0] [1  1]]
        seat? (set seats)
        neighbors #(keep (comp seat? (partial seat-lookup floor? %)) directions)]
    (->> seats
         (map #(vector % (neighbors %)))
         (into {})
         (assoc m :neighbors))))

(defn- steps [tolerance {:keys [occupied? seats neighbors]}]
  (->> occupied?
       (iterate
        (fn [occupied?]
          (->> seats
               (filter
                #(if (occupied? %)
                   (not= tolerance (->> (neighbors %) (filter occupied?) (bounded-count tolerance)))
                   (not-any? occupied? (neighbors %))))
               set)))))

(defn- first-repeat [xs]
  (->> (partition 2 1 xs)
       (drop-while #(apply not= %))
       ffirst))

(defn- stable-occupation [input seat-lookup tolerance]
  (->> (parse-map input)
       (precompute-neighbors seat-lookup)
       (steps tolerance)
       first-repeat
       count))

(defn part1 [input]
  (stable-occupation input neighbor 4))

(defn part2 [input]
  (stable-occupation input first-seen 5))
