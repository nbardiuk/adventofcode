(ns day08
  (:require [clojure.string :as str]))

(defn parse-grid [input]
  (->> (str/split-lines input)
       (mapv #(->> (re-seq #"\d" %)
                   (mapv parse-long)))))

(defn enumerate [xs]
  (map vector (range) xs))

(defn transpose [rows]
  (apply mapv vector rows))

(defn tree-view-seq [rows]
  (let [columns (transpose rows)]
    (for [[r row]    (enumerate rows)
          [c height] (enumerate row)]
      (let [column (get columns c)
            left   (->> row (take c) reverse)
            right  (->> row (drop (inc c)))
            top    (->> column (take r) reverse)
            bottom (->> column (drop (inc r)))]
        [height [left right top bottom]]))))

(defn every-smaller? [height trees]
  (every? #(< % height) trees))

(defn count-visible [height trees]
  (reduce
   (fn [visible tree]
     (if (< tree height)
       (inc visible)
       (reduced (inc visible))))
   0 trees))

(defn part1 [input]
  (let [grid (parse-grid input)]
    (->> (for [[height sides] (tree-view-seq grid)
               :when (some #(every-smaller? height %) sides)]
           :visible)
         count)))

(defn part2 [input]
  (let [grid (parse-grid input)]
    (->> (for [[height sides] (tree-view-seq grid)]
           (->> sides
                (map #(count-visible height %))
                (reduce *)))
         (reduce max))))
