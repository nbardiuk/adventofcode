(ns day04
  (:require
    [clojure.string :as str]))

(defn- read-grid [input]
  (->> (for [[y line] (map vector (range) (str/split-lines input))
             [x ch] (map vector (range) line)]
         [[x y] ch])
       (into {})))

(defn part1 [input]
  (let [grid (read-grid input)
        xs (->> grid (filter #(= \X (val %))) (map key))]
    (count (for [[x y] xs
                 dx [-1 0 1]
                 dy [-1 0 1]
                 :when (and (= \M (grid [(+ x (* 1 dx)) (+ y (* 1 dy))]))
                            (= \A (grid [(+ x (* 2 dx)) (+ y (* 2 dy))]))
                            (= \S (grid [(+ x (* 3 dx)) (+ y (* 3 dy))])))]
             :xmas))))

(defn part2 [input]
  (let [grid (read-grid input)
        as (->> grid (filter #(= \A (val %))) (map key))]
    (count (for [[x y] as
                 [[dx dy] [dx' dy']] [[[1 1] [1 -1]]
                                      [[1 1] [-1 1]]
                                      [[-1 -1] [1 -1]]
                                      [[-1 -1] [-1 1]]]
                 :when (and (= \M
                               (grid [(+ x (* 1 dx)) (+ y (* 1 dy))])
                               (grid [(+ x (* 1 dx')) (+ y (* 1 dy'))]))
                            (= \S
                              (grid [(+ x (* -1 dx)) (+ y (* -1 dy))])
                              (grid [(+ x (* -1 dx')) (+ y (* -1 dy'))])))]
             :xmas))))
