(ns day12
  (:require [clojure.string :as str])
  (:import [java.util PriorityQueue]))

(defn enumerate [xs]
  (map vector (range) xs))

(defn find-first [p xs]
  (first (filter p xs)))

(defn parse-grid [input]
  (->> (for [[y line] (enumerate (str/split-lines input))
             [x c] (enumerate line)]
         [[x y] c])
       (into {})))

(defn read-map [input]
  (let [grid (parse-grid input)
        [S] (find-first (comp #{\S} val) grid)
        [E] (find-first (comp #{\E} val) grid)]
    {:S S
     :E E
     :grid (assoc grid S \a E \z)}))

(defn neighbours [grid [x y]]
  (->> [[-1 0] [1 0] [0 -1] [0 1]]
       (keep (fn [[dx dy]]
               (find grid [(+ x dx) (+ y dy)])))))

(defn path-len [grid start end? wall?]
  (let [start (find grid start)
        queue (new PriorityQueue 100 (comparator (fn [[a] [b]] (< (count a) (count b)))))]
    (.add queue [[] start])
    (loop [seen? #{start}]
      (let [[path [pos :as current]] (.poll queue)]
        (if (end? current)
          (count path)
          (let [ns (for [next (neighbours grid pos)
                         :when (and (not (seen? next))
                                    (not (wall? current next)))]
                     [(conj path next) next])]
            (.addAll queue ns)
            (recur (into seen? (map second ns)))))))))

(defn part1 [input]
  (let [{:keys [grid S E]} (read-map input)]
    (path-len grid S
              (fn [[position]] (= E position))
              (fn [[_ level] [_ next-level]]
                (< 1 (- (long next-level) (long level)))))))

(defn part2 [input]
  (let [{:keys [grid E]} (read-map input)]
    (path-len grid E
              (fn [[_ level]] (= \a level))
              (fn [[_ level] [_ next-level]]
                (< 1 (- (long level) (long next-level)))))))
